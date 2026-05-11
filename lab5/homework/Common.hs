{-# LANGUAGE OverloadedStrings #-}

module Common where

import Control.Monad (forM_, void)
import Data.Char (toLower)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.AMQP
import Text.Read (readMaybe)

data Service = People | Cargo | Satellite
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

allServices :: [Service]
allServices = [minBound .. maxBound]

serviceFromString :: String -> Maybe Service
serviceFromString raw =
  case map toLower raw of
    "people" -> Just People
    "cargo" -> Just Cargo
    "satellite" -> Just Satellite
    _ -> Nothing

serviceRoutingKey :: Service -> Text
serviceRoutingKey People = "people"
serviceRoutingKey Cargo = "cargo"
serviceRoutingKey Satellite = "satellite"

serviceRoutingKeyString :: Service -> String
serviceRoutingKeyString = T.unpack . serviceRoutingKey

serviceQueueName :: Service -> Text
serviceQueueName People = "service.people"
serviceQueueName Cargo = "service.cargo"
serviceQueueName Satellite = "service.satellite"

supportedServicesText :: String
supportedServicesText = intercalate "|" (map serviceRoutingKeyString allServices)

ordersExchange :: Text
ordersExchange = "space.orders"

confirmExchange :: Text
confirmExchange = "space.confirm"

eventsExchange :: Text
eventsExchange = "space.events"

adminAgenciesExchange :: Text
adminAgenciesExchange = "space.admin.agencies"

adminCarriersExchange :: Text
adminCarriersExchange = "space.admin.carriers"

agencyConfirmQueue :: String -> Text
agencyConfirmQueue agency = T.pack ("agency." ++ agency ++ ".confirm")

agencyAdminQueue :: String -> Text
agencyAdminQueue agency = T.pack ("agency." ++ agency ++ ".admin")

carrierAdminQueue :: String -> Text
carrierAdminQueue carrier = T.pack ("carrier." ++ carrier ++ ".admin")

adminAuditQueue :: Text
adminAuditQueue = "admin.audit"

data Order = Order
  { orderAgency :: String
  , orderId :: String
  , orderService :: Service
  , orderPayload :: String
  , orderCreatedAt :: String
  }
  deriving (Show, Read)

data Confirmation = Confirmation
  { confirmAgency :: String
  , confirmOrderId :: String
  , confirmService :: Service
  , confirmCarrier :: String
  , confirmStatus :: String
  , confirmCreatedAt :: String
  }
  deriving (Show, Read)

encodeRecord :: Show a => a -> BL.ByteString
encodeRecord = BL.pack . show

decodeRecord :: Read a => BL.ByteString -> Maybe a
decodeRecord = readMaybe . BL.unpack

publishRecord :: Show a => Channel -> Text -> Text -> a -> IO ()
publishRecord ch exchangeName routingKey payload =
  void $
    publishMsg ch exchangeName routingKey newMsg
      { msgBody = encodeRecord payload
      , msgDeliveryMode = Just Persistent
      }

publishText :: Channel -> Text -> Text -> String -> IO ()
publishText ch exchangeName routingKey textPayload =
  void $
    publishMsg ch exchangeName routingKey newMsg
      { msgBody = BL.pack textPayload
      , msgDeliveryMode = Just Persistent
      }

setupTopology :: Channel -> IO ()
setupTopology ch = do
  declareExchange ch newExchange
    { exchangeName = ordersExchange
    , exchangeType = "direct"
    , exchangeDurable = True
    }
  declareExchange ch newExchange
    { exchangeName = confirmExchange
    , exchangeType = "direct"
    , exchangeDurable = True
    }
  declareExchange ch newExchange
    { exchangeName = eventsExchange
    , exchangeType = "topic"
    , exchangeDurable = True
    }
  declareExchange ch newExchange
    { exchangeName = adminAgenciesExchange
    , exchangeType = "fanout"
    , exchangeDurable = True
    }
  declareExchange ch newExchange
    { exchangeName = adminCarriersExchange
    , exchangeType = "fanout"
    , exchangeDurable = True
    }

  forM_ allServices $ \service -> do
    let queueName = serviceQueueName service
        routingKey = serviceRoutingKey service
    _ <- declareQueue ch newQueue
      { queueName = queueName
      , queueDurable = True
      }
    bindQueue ch queueName ordersExchange routingKey

declareAgencyQueues :: Channel -> String -> IO (Text, Text)
declareAgencyQueues ch agency = do
  let confirmQueue = agencyConfirmQueue agency
      adminQueue = agencyAdminQueue agency

  _ <- declareQueue ch newQueue
    { queueName = confirmQueue
    , queueDurable = True
    }
  bindQueue ch confirmQueue confirmExchange (T.pack agency)

  _ <- declareQueue ch newQueue
    { queueName = adminQueue
    , queueDurable = True
    }
  bindQueue ch adminQueue adminAgenciesExchange ""
  pure (confirmQueue, adminQueue)

declareCarrierAdminQueue :: Channel -> String -> IO Text
declareCarrierAdminQueue ch carrier = do
  let queueName = carrierAdminQueue carrier
  _ <- declareQueue ch newQueue
    { queueName = queueName
    , queueDurable = True
    }
  bindQueue ch queueName adminCarriersExchange ""
  pure queueName

declareAdminAuditQueue :: Channel -> IO Text
declareAdminAuditQueue ch = do
  _ <- declareQueue ch newQueue
    { queueName = adminAuditQueue
    , queueDurable = True
    }
  bindQueue ch adminAuditQueue eventsExchange "#"
  pure adminAuditQueue

nowUtcString :: IO String
nowUtcString = do
  now <- getCurrentTime
  pure (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)

formatOrderRef :: Order -> String
formatOrderRef order = orderAgency order ++ "#" ++ orderId order

renderOrderEvent :: Order -> String
renderOrderEvent order =
  "ORDER " ++ formatOrderRef order
    ++ " service="
    ++ serviceRoutingKeyString (orderService order)
    ++ " payload=\""
    ++ orderPayload order
    ++ "\""

renderConfirmationEvent :: Confirmation -> String
renderConfirmationEvent confirmation =
  "CONFIRMATION "
    ++ confirmAgency confirmation
    ++ "#"
    ++ confirmOrderId confirmation
    ++ " service="
    ++ serviceRoutingKeyString (confirmService confirmation)
    ++ " carrier="
    ++ confirmCarrier confirmation
    ++ " status="
    ++ confirmStatus confirmation

renderAdminEvent :: String -> String -> String
renderAdminEvent target messageBody =
  "ADMIN target=" ++ target ++ " message=\"" ++ messageBody ++ "\""
