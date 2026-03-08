import System.Environment (getArgs)
import Data.Maybe (fromMaybe, listToMaybe)
import Text.Read (readMaybe)
import Data.List (intercalate)
import Control.Monad (when)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan, dupChan)
import Control.Concurrent (forkIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 (getLine, pack)
import qualified Data.ByteString (null, putStr, append, concat, isPrefixOf) 
import Network.Socket (Socket, socket, bind, listen, accept, close, connect, Family(AF_INET), SocketType(Stream, Datagram), SockAddr(SockAddrInet), tupleToHostAddress, defaultPort, socketPort)
import Network.Socket.ByteString (recv, sendAll, recvFrom, sendAllTo)
import Network.Multicast (multicastSender, multicastReceiver)

tcpReceiver :: Socket -> IO()
tcpReceiver sock = do
  received <- recv sock 1024
  if Data.ByteString.null received then
    close sock
  else do
    _ <- Data.ByteString.putStr $ Data.ByteString.append received $ Data.ByteString.Char8.pack "\n"
    tcpReceiver sock

udpReceiver :: Socket -> IO ()
udpReceiver sock = do
  received <- recv sock 1024
  _ <- Data.ByteString.putStr $ Data.ByteString.append received $ Data.ByteString.Char8.pack "\n"
  udpReceiver sock

multiReceiver :: Socket -> ByteString -> IO ()
multiReceiver sock nick = do
  (received, _) <- recvFrom sock 1024
  when (not $ Data.ByteString.isPrefixOf (Data.ByteString.concat [nick, Data.ByteString.Char8.pack " from multicast:", image]) received) $ Data.ByteString.putStr $ Data.ByteString.append received $ Data.ByteString.Char8.pack "\n"
  multiReceiver sock nick

sender :: Socket -> Socket -> Socket -> ByteString -> SockAddr -> IO()
sender tcpSock udpSock multicastSock nick addr = do
  toSend <- Data.ByteString.Char8.getLine
  if toSend == Data.ByteString.Char8.pack "U" then
    sendAll udpSock $ Data.ByteString.concat [nick, Data.ByteString.Char8.pack " from UDP:\n", image]
  else if toSend == Data.ByteString.Char8.pack "M" then
    sendAllTo multicastSock (Data.ByteString.concat [nick, Data.ByteString.Char8.pack " from multicast:\n", image]) addr
  else
    sendAll tcpSock toSend
  sender tcpSock udpSock multicastSock nick addr
  
main = do
  args <- getArgs  
  case readMaybe . fromMaybe "" . listToMaybe $ args of
    Nothing -> putStrLn "Incorrect port"
    Just port -> do
      case args of
        (_:n:ns) -> do
          tcpSock <- socket AF_INET Stream 0
          connect tcpSock $ SockAddrInet port $ tupleToHostAddress (127, 0, 0, 1)
          _ <- sendAll tcpSock $ Data.ByteString.Char8.pack $ intercalate " " (n:ns)
          boundPort <- socketPort tcpSock 
          udpSock <- socket AF_INET Datagram 0
          bind udpSock $ SockAddrInet boundPort $ tupleToHostAddress (127, 0, 0, 1)
          connect udpSock $ SockAddrInet port $ tupleToHostAddress (127, 0, 0, 1)
          (multiSock, addr) <- multicastSender "224.0.0.99" 9999
          multiReceiveSock <- multicastReceiver "224.0.0.99" 9999 
          forkIO $ sender tcpSock udpSock multiSock (Data.ByteString.Char8.pack $ intercalate " " (n:ns)) addr
          forkIO $ multiReceiver multiReceiveSock $ Data.ByteString.Char8.pack $ intercalate " " (n:ns)
          forkIO $ udpReceiver udpSock
          tcpReceiver tcpSock
        _ -> putStrLn "Enter nick"

image :: ByteString
image = Data.ByteString.Char8.pack "\
\                 ;i.\n\
\                  M$L                    .;i.\n\
\                  M$Y;                .;iii;;.\n\
\                 ;$YY$i._           .iiii;;;;;\n\
\                .iiiYYYYYYiiiii;;;;i;iii;; ;;;\n\
\              .;iYYYYYYiiiiiiYYYiiiiiii;;  ;;;\n\
\           .YYYY$$$$YYYYYYYYYYYYYYYYiii;; ;;;;\n\
\         .YYY$$$$$$YYYYYY$$$$iiiY$$$$$$$ii;;;;\n\
\        :YYYF`,  TYYYYY$$$$$YYYYYYYi$$$$$iiiii;\n\
\        Y$MM: \\  :YYYY$$P\"````T$YYMMMMMMMMiiYY.\n\
\     `.;$$M$$b.,dYY$$Yi; .(     .YYMMM$$$MMMMYY\n\
\   .._$MMMMM$!YYYYYYYYYi;.`\"  .;iiMMM$MMMMMMMYY\n\
\    ._$MMMP` ```\"\"4$$$$$iiiiiiii$MMMMMMMMMMMMMY;\n\
\     MMMM$:       :$$$$$$$MMMMMMMMMMM$$MMMMMMMYYL\n\
\    :MMMM$$.    .;PPb$$$$MMMMMMMMMM$$$$MMMMMMiYYU:\n\
\     iMM$$;;: ;;;;i$$$$$$$MMMMM$$$$MMMMMMMMMMYYYYY\n\
\     `$$$$i .. ``:iiii!*\"``.$$$$$$$$$MMMMMMM$YiYYY\n\
\      :Y$$iii;;;.. ` ..;;i$$$$$$$$$MMMMMM$$YYYYiYY:\n\
\       :$$$$$iiiiiii$$$$$$$$$$$MMMMMMMMMMYYYYiiYYYY.\n\
\        `$$$$$$$$$$$$$$$$$$$$MMMMMMMM$YYYYYiiiYYYYYY\n\
\         YY$$$$$$$$$$$$$$$$MMMMMMM$$YYYiiiiiiYYYYYYY\n\
\        :YYYYYY$$$$$$$$$$$$$$$$$$YYYYYYYiiiiYYYYYYi\'\n"
