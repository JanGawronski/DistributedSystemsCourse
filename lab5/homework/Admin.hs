module Main where

import Common
import Data.Char (toLower)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Network.AMQP
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  conn <- openConnection "127.0.0.1" (T.pack "/") (T.pack "guest") (T.pack "guest")
  ch <- openChannel conn
  setupTopology ch
  auditQueue <- declareAdminAuditQueue ch

  _ <- consumeMsgs ch auditQueue NoAck $ \(msg, _) ->
    putStrLn $ "[AUDIT] " ++ BL.unpack (msgBody msg)

  putStrLn "Admin is ready."
  putStrLn "Commands: agencies <message> | carriers <message> | both <message> | exit"
  adminLoop ch

  closeChannel ch
  closeConnection conn

adminLoop :: Channel -> IO ()
adminLoop ch = do
  putStr "admin> "
  hFlush stdout
  command <- getLine
  case words command of
    [] -> adminLoop ch
    ["exit"] -> pure ()
    (targetToken : messageTokens)
      | not (null messageTokens) -> do
          let target = map toLower targetToken
              messageBody = unwords messageTokens
          case target of
            "agencies" -> do
              publishText ch adminAgenciesExchange T.empty messageBody
              publishText ch eventsExchange (T.pack "admin.agencies") (renderAdminEvent "agencies" messageBody)
              putStrLn "Broadcast sent to all agencies."
            "carriers" -> do
              publishText ch adminCarriersExchange T.empty messageBody
              publishText ch eventsExchange (T.pack "admin.carriers") (renderAdminEvent "carriers" messageBody)
              putStrLn "Broadcast sent to all carriers."
            "both" -> do
              publishText ch adminAgenciesExchange T.empty messageBody
              publishText ch adminCarriersExchange T.empty messageBody
              publishText ch eventsExchange (T.pack "admin.both") (renderAdminEvent "both" messageBody)
              putStrLn "Broadcast sent to all agencies and all carriers."
            _ ->
              putStrLn "Unknown target. Use: agencies | carriers | both."
          adminLoop ch
    _ -> do
      putStrLn "Invalid command."
      adminLoop ch
