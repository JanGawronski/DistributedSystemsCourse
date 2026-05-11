module Main where

import Common
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Network.AMQP
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  (carrierName, services) <-
    case args of
      [name, s1, s2] ->
        case (serviceFromString s1, serviceFromString s2) of
          (Just svc1, Just svc2)
            | svc1 /= svc2 -> pure (name, [svc1, svc2])
            | otherwise -> die "Carrier must support exactly two different services."
          _ ->
            die $
              "Unknown service. Use exactly two from: " ++ supportedServicesText
      _ ->
        die "Usage: Carrier <carrier-name> <service-1> <service-2>"

  conn <- openConnection "127.0.0.1" (T.pack "/") (T.pack "guest") (T.pack "guest")
  ch <- openChannel conn
  setupTopology ch
  adminQueue <- declareCarrierAdminQueue ch carrierName

  _ <- consumeMsgs ch adminQueue NoAck $ \(msg, _) ->
    putStrLn $
      "[ADMIN -> CARRIERS][" ++ carrierName ++ "] " ++ BL.unpack (msgBody msg)

  forM_ services $ \service -> do
    let queueName = serviceQueueName service
    _ <- consumeMsgs ch queueName Ack $ handleOrder ch carrierName service
    pure ()

  putStrLn $
    "Carrier '"
      ++ carrierName
      ++ "' is ready for: "
      ++ unwords (map serviceRoutingKeyString services)
  waitForever

handleOrder :: Channel -> String -> Service -> (Message, Envelope) -> IO ()
handleOrder ch carrierName expectedService (msg, env) =
  case decodeRecord (msgBody msg) of
    Nothing -> do
      putStrLn $
        "["
          ++ carrierName
          ++ "] Failed to decode order message: "
          ++ BL.unpack (msgBody msg)
      ackEnv env
    Just order -> do
      let realService = orderService order
      if realService /= expectedService
        then
          putStrLn $
            "["
              ++ carrierName
              ++ "] Warning: got order from unexpected queue: "
              ++ formatOrderRef order
        else do
          putStrLn $
            "["
              ++ carrierName
              ++ "] Handling "
              ++ formatOrderRef order
              ++ " ("
              ++ serviceRoutingKeyString realService
              ++ ")"
          timestamp <- nowUtcString
          let confirmation =
                Confirmation
                  { confirmAgency = orderAgency order
                  , confirmOrderId = orderId order
                  , confirmService = realService
                  , confirmCarrier = carrierName
                  , confirmStatus = "done"
                  , confirmCreatedAt = timestamp
                  }
          publishRecord ch confirmExchange (T.pack (orderAgency order)) confirmation
          publishText ch eventsExchange (T.pack ("confirm." ++ serviceRoutingKeyString realService)) (renderConfirmationEvent confirmation)
          putStrLn $
            "["
              ++ carrierName
              ++ "] Confirmation sent for "
              ++ formatOrderRef order
      ackEnv env

waitForever :: IO ()
waitForever = do
  threadDelay 1000000
  waitForever
