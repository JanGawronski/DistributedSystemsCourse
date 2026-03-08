import System.Environment (getArgs)
import Data.Maybe (fromMaybe, listToMaybe)
import Text.Read (readMaybe)
import Control.Monad (when)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan, dupChan)
import Control.Concurrent (forkIO)
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)
import qualified Data.ByteString (null, concat) 
import Network.Socket (Socket, socket, bind, listen, accept, close, Family(AF_INET), SocketType(Stream, Datagram), SockAddr(SockAddrInet), tupleToHostAddress, defaultPort, socketPort, connect)
import Network.Socket.ByteString (recv, recvFrom, sendAll, sendAllTo)

dispatcher :: Socket -> Socket -> SockAddr -> Chan (SockAddr, ByteString, ByteString) -> Chan (SockAddr, ByteString) -> IO ()
dispatcher tcpSock udpSock addr tcpChannel udpChannel = do
  nick <- recv tcpSock 1024
  forkIO $ tcpReceiver tcpSock addr tcpChannel nick
  udpSendChan <- dupChan udpChannel
  forkIO $ udpSender udpSock addr udpSendChan
  tcpSendChan <- dupChan tcpChannel
  tcpSender tcpSock addr tcpSendChan

tcpReceiver :: Socket -> SockAddr -> Chan (SockAddr, ByteString, ByteString) -> ByteString -> IO ()
tcpReceiver sock addr channel nick = do
   received <- recv sock 1024
   writeChan channel (addr, nick, received)
   if Data.ByteString.null received then
     close sock
   else
     tcpReceiver sock addr channel nick 

tcpSender :: Socket -> SockAddr -> Chan (SockAddr, ByteString, ByteString) -> IO ()
tcpSender sock addr channel = do
  (sender, nick, toSend) <- readChan channel  
  when (addr /= sender && (not $ Data.ByteString.null toSend)) $ sendAll sock $ Data.ByteString.concat [nick, pack ": ", toSend]
  when (addr /= sender || (not $ Data.ByteString.null toSend)) $ tcpSender sock addr channel

udpReceiver :: Socket -> Chan (SockAddr, ByteString) -> IO ()
udpReceiver sock channel = do
  (received, addr) <- recvFrom sock 1024
  writeChan channel (addr, received)
  udpReceiver sock channel 

udpSender :: Socket -> SockAddr -> Chan (SockAddr, ByteString) -> IO ()
udpSender sock addr channel = do
  (sender, toSend) <- readChan channel
  when (addr /= sender) $ sendAllTo sock toSend addr
  udpSender sock addr channel 

acceptor :: Socket -> Socket -> Chan (SockAddr, ByteString, ByteString) -> Chan (SockAddr, ByteString) -> IO ()
acceptor tcpSock udpSock tcpChannel udpChannel = do
  (newSock, addr) <- accept tcpSock 
  forkIO $ dispatcher newSock udpSock addr tcpChannel udpChannel
  acceptor tcpSock udpSock tcpChannel udpChannel 
  
main = do
  args <- getArgs
  let port = fromMaybe defaultPort . readMaybe . fromMaybe "0" . listToMaybe $ args
  tcpChannel <- newChan
  tcpSock <- socket AF_INET Stream 0
  bind tcpSock $ SockAddrInet port $ tupleToHostAddress (127, 0, 0, 1)
  listen tcpSock 5
  boundPort <- socketPort tcpSock 
  when (port == defaultPort) $ putStrLn $ "Server available at " ++ show boundPort
  udpSock <- socket AF_INET Datagram 0
  bind udpSock $ SockAddrInet boundPort $ tupleToHostAddress (127, 0, 0, 1)
  udpChannel <- newChan
  forkIO $ udpReceiver udpSock udpChannel
  acceptor tcpSock udpSock tcpChannel udpChannel
