{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO, newChan, newMVar, readChan, withMVar, writeChan, MVar)
import Control.Exception (SomeException, catch, displayException)
import Control.Monad (forever, unless, void, when)
import Data.Foldable qualified as F
import Data.IORef
import Data.Maybe (isNothing)
import Data.String (fromString)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hFlush, isEOF, stderr, stdout, putStrLn, hPutStrLn)
import System.Process
  ( CreateProcess(std_err, std_in, std_out)
  , ProcessHandle
  , StdStream(NoStream)
  , createProcess
  , getProcessExitCode
  , proc
  , shell
  , terminateProcess
  )
import ZooKeeper
import qualified ZooKeeper.Types as ZK
import qualified Z.Data.CBytes as CB

data Config = Config
  { zkConnect :: String
  , guiProgram :: FilePath
  , guiArgs :: [String]
  , notifyTemplate :: String
  }

defaultNotifyTemplate :: String
defaultNotifyTemplate =
  "xmessage -center -timeout 2 'Number of children /a: %COUNT%'"

usage :: String
usage =
  unlines
    [ "Usage:"
    , "  a-watch <zk-hosts> <gui-cmd> [gui-args...] [--notify-template <shell-template>]"
    , ""
    , "Przykład:"
    , "  a-watch 127.0.0.1:2181,127.0.0.1:2182,127.0.0.1:2183 xeyes"
    , "  a-watch 127.0.0.1:2181,127.0.0.1:2182,127.0.0.1:2183 xeyes"
    , "    --notify-template \"xmessage -center -timeout 2 'Number of children /a: %COUNT%'\""
    ]

parseArgs :: [String] -> Either String Config
parseArgs (conn : gui : rest) =
  let (guiArgs0, tailArgs) = break (== "--notify-template") rest
  in case tailArgs of
       [] ->
         Right Config
           { zkConnect = conn
           , guiProgram = gui
           , guiArgs = guiArgs0
           , notifyTemplate = defaultNotifyTemplate
           }
       [_] ->
         Left "No template after --notify-template."
       (_flag : tpl : extra)
         | null extra ->
             Right Config
               { zkConnect = conn
               , guiProgram = gui
               , guiArgs = guiArgs0
               , notifyTemplate = tpl
               }
         | otherwise ->
             Left "Too many arguments after --notify-template."
parseArgs _ = Left usage

main :: IO ()
main = do
  argv <- getArgs
  cfg <- either (die . ("Error parsing arguments:\n" <>)) pure (parseArgs argv)

  refreshQueue <- newChan
  zkLock <- newMVar ()
  guiHandleRef <- newIORef Nothing
  lastCountRef <- newIORef Nothing

  let watcher :: Maybe ZK.WatcherFn
      watcher = Nothing

  let resource = zookeeperResInit (fromString (zkConnect cfg)) watcher 10000 Nothing 0

  withResource resource $ \zh -> do
    let signalRefresh = writeChan refreshQueue ()

    _ <- forkIO $ forever $ do
      readChan refreshQueue
      withMVar zkLock $ \_ ->
        refresh zh cfg guiHandleRef lastCountRef signalRefresh
          `catch` \(e :: SomeException) ->
            hPutStrLn stderr ("refresh error: " <> displayException e)

    signalRefresh
    repl zh zkLock

refresh
  :: ZK.ZHandle
  -> Config
  -> IORef (Maybe ProcessHandle)
  -> IORef (Maybe Int)
  -> IO ()
  -> IO ()
refresh zh cfg guiHandleRef lastCountRef signalRefresh = do
  mStat <- zooExists zh "/a"
  case mStat of
    Nothing -> do
      stopGui guiHandleRef
      writeIORef lastCountRef Nothing
      armExistsWatch zh signalRefresh
    Just _ -> do
      ensureGuiRunning cfg guiHandleRef
      armChildrenWatch zh cfg lastCountRef signalRefresh

armExistsWatch :: ZK.ZHandle -> IO () -> IO ()
armExistsWatch zh signalRefresh =
  zooWatchExists zh "/a"
    (\_ -> signalRefresh)
    (\_ -> pure ())

armChildrenWatch
  :: ZK.ZHandle
  -> Config
  -> IORef (Maybe Int)
  -> IO ()
  -> IO ()
armChildrenWatch zh cfg lastCountRef signalRefresh =
  zooWatchGetChildren zh "/a"
    (\_ -> signalRefresh)
    (\(ZK.StringsCompletion (ZK.StringVector kids)) -> do
        let count = F.length kids
        old <- readIORef lastCountRef
        when (shouldNotify old count) $
          notifyCount cfg count
        writeIORef lastCountRef (Just count)
    )
  `catch` \(e :: SomeException) -> do
    hPutStrLn stderr ("watch children error: " <> displayException e)
    armExistsWatch zh signalRefresh

shouldNotify :: Maybe Int -> Int -> Bool
shouldNotify Nothing n = n > 0
shouldNotify (Just old) n = n > old

ensureGuiRunning :: Config -> IORef (Maybe ProcessHandle) -> IO ()
ensureGuiRunning Config{..} ref = do
  m <- readIORef ref
  alive <- case m of
    Nothing -> pure False
    Just ph -> isNothing <$> getProcessExitCode ph

  unless alive $ do
    (_, _, _, ph) <-
      createProcess
        (proc guiProgram guiArgs)
          { std_in = NoStream
          , std_out = NoStream
          , std_err = NoStream
          }
    writeIORef ref (Just ph)

stopGui :: IORef (Maybe ProcessHandle) -> IO ()
stopGui ref = do
  m <- readIORef ref
  case m of
    Nothing -> pure ()
    Just ph -> do
      terminateProcess ph `catch` \(_ :: SomeException) -> pure ()
      writeIORef ref Nothing

notifyCount :: Config -> Int -> IO ()
notifyCount Config{..} count = do
  let cmdLine = replaceCount notifyTemplate (show count)
  void $
    createProcess
      (shell cmdLine)
        { std_in = NoStream
        , std_out = NoStream
        , std_err = NoStream
        }

replaceCount :: String -> String -> String
replaceCount template value = go template
  where
    go [] = []
    go ('%' : 'C' : 'O' : 'U' : 'N' : 'T' : '%' : xs) = value <> go xs
    go (x : xs) = x : go xs

dumpTree :: ZK.ZHandle -> CB.CBytes -> Int -> IO ()
dumpTree zh root depth =
  (go root depth) `catch` \(e :: SomeException) ->
    hPutStrLn stderr ("tree error: " <> displayException e)
  where
    go :: CB.CBytes -> Int -> IO ()
    go path level = do
      mStat <- zooExists zh path
      case mStat of
        Nothing ->
          putStrLn (replicate (level * 2) ' ' <> CB.unpack path <> " [no elements]")
        Just _ -> do
          putStrLn (replicate (level * 2) ' ' <> CB.unpack path)
          ZK.StringsCompletion (ZK.StringVector kids) <- zooGetChildren zh path
          F.forM_ (F.toList kids) $ \child ->
            go (path <> "/" <> child) (level + 1)

repl :: ZK.ZHandle -> MVar () -> IO ()
repl zh zkLock = do
  putStrLn "Commands: tree | quit | help"
  loop
  where
    loop = do
      putStr "> "
      hFlush stdout
      eof <- isEOF
      if eof
        then pure ()
        else do
          line <- getLine
          case words line of
            ["quit"] -> pure ()
            ["tree"]  -> withMVar zkLock $ \_ -> dumpTree zh "/a" 0
            ["help"]  -> putStrLn "tree = print tree /a; quit = quit program" >> loop
            []        -> loop
            _         -> putStrLn "Unknown Command. Usage: tree, quit, help." >> loop
