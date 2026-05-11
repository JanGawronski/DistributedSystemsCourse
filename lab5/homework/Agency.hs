module Main where

import Common
import Data.Char (toLower)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Network.AMQP
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  agencyName <-
    case args of
      [name] -> pure name
      _ -> die "Usage: Agency <agency-name>"

  conn <- openConnection "127.0.0.1" (T.pack "/") (T.pack "guest") (T.pack "guest")
  ch <- openChannel conn
  setupTopology ch
  (confirmQueue, adminQueue) <- declareAgencyQueues ch agencyName

  _ <- consumeMsgs ch confirmQueue Ack $ \(msg, env) -> do
    case decodeRecord (msgBody msg) of
      Just confirmation ->
        putStrLn $
          "[CONFIRMATION] "
            ++ confirmAgency confirmation
            ++ "#"
            ++ confirmOrderId confirmation
            ++ " handled by "
            ++ confirmCarrier confirmation
            ++ " ("
            ++ serviceRoutingKeyString (confirmService confirmation)
            ++ ")"
      Nothing ->
        putStrLn $
          "[CONFIRMATION] Failed to decode message: "
            ++ BL.unpack (msgBody msg)
    ackEnv env

  _ <- consumeMsgs ch adminQueue NoAck $ \(msg, _) ->
    putStrLn $ "[ADMIN -> AGENCIES] " ++ BL.unpack (msgBody msg)

  putStrLn $ "Agency '" ++ agencyName ++ "' is ready."
  putStrLn $
    "Enter order: <order-id> <service("
      ++ supportedServicesText
      ++ ")> <details> or 'exit'"
  sendOrdersLoop ch agencyName

  closeChannel ch
  closeConnection conn

sendOrdersLoop :: Channel -> String -> IO ()
sendOrdersLoop ch agencyName = do
  putStr "> "
  command <- getLine
  if map toLower command == "exit"
    then pure ()
    else do
      case words command of
        (orderIdToken : serviceToken : detailsTokens)
          | not (null detailsTokens) ->
              case serviceFromString serviceToken of
                Nothing ->
                  putStrLn $
                    "Unknown service. Use one of: " ++ supportedServicesText
                Just service -> do
                  timestamp <- nowUtcString
                  let details = unwords detailsTokens
                      order =
                        Order
                          { orderAgency = agencyName
                          , orderId = orderIdToken
                          , orderService = service
                          , orderPayload = details
                          , orderCreatedAt = timestamp
                  }
                  publishRecord ch ordersExchange (serviceRoutingKey service) order
                  publishText ch eventsExchange (T.pack ("order." ++ serviceRoutingKeyString service)) (renderOrderEvent order)
                  putStrLn $
                    "[ORDER SENT] " ++ formatOrderRef order ++ " -> " ++ serviceRoutingKeyString service
        _ ->
          putStrLn "Invalid input. Expected: <order-id> <service> <details>."
      sendOrdersLoop ch agencyName
