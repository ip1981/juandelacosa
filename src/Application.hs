{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Application
  ( app
  ) where

import Control.Monad.Trans (liftIO)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Default.Class (def)
import Data.Pool (Pool, withResource)
import Data.Text.Lazy (Text, toLower)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Database.MySQL.Simple (Connection, Only(..), execute, query)
import Network.HTTP.Types (badRequest400, notFound404)
import Network.Wai (Application, Middleware)
import Network.Wai.Middleware.RequestLogger
  ( Destination(Handle)
  , OutputFormat(CustomOutputFormat)
  , RequestLoggerSettings(destination, outputFormat)
  , mkRequestLogger
  )
import Network.Wai.Middleware.Static ((>->), addBase, hasPrefix, staticPolicy)
import System.Entropy (getEntropy)
import System.IO (stderr)
import Web.Scotty
  ( ActionM
  , ScottyM
  , file
  , get
  , header
  , middleware
  , post
  , scottyApp
  , status
  , text
  )

import LogFormat (logFormat)

app :: Pool Connection -> FilePath -> IO Application
app p f = do
  logger <-
    mkRequestLogger
      def
        { destination = Handle stderr
        , outputFormat = CustomOutputFormat logFormat
        }
  scottyApp (juanDeLaCosa p logger f)

juanDeLaCosa :: Pool Connection -> Middleware -> FilePath -> ScottyM ()
juanDeLaCosa p logger dataDir = do
  middleware logger
  middleware $ staticPolicy (hasPrefix "static" >-> addBase dataDir)
  get "/" $ file (dataDir ++ "/" ++ "index.html")
  post "/resetMyPassword" $ apiResetMyPassword p
  get "/whoAmI" $ apiWhoAmI p

apiWhoAmI :: Pool Connection -> ActionM ()
apiWhoAmI p =
  header "From" >>= \case
    Nothing -> status badRequest400 >> text "Missing header `From'"
    Just email -> do
      let login = emailToLogin email
      [Only n] <-
        withDB p $ \c ->
          query
            c
            "SELECT COUNT(*) FROM mysql.user WHERE User=? AND Host='%'"
            [LBS.toStrict . encodeUtf8 $ login]
      if (n :: Int) > 0
        then text login
        else status notFound404 >> text login

apiResetMyPassword :: Pool Connection -> ActionM ()
apiResetMyPassword p =
  header "From" >>= \case
    Nothing -> status badRequest400 >> text "Missing header `From'"
    Just email -> do
      let login = emailToLogin email
      password <- liftIO $ BS.takeWhile (/= '=') . encode <$> getEntropy 13
      _ <-
        withDB p $ \c ->
          execute
            c
            "SET PASSWORD FOR ?@'%' = PASSWORD(?)"
            [LBS.toStrict . encodeUtf8 $ login, password]
      text . decodeUtf8 . LBS.fromStrict $ password

withDB :: Pool Connection -> (Connection -> IO a) -> ActionM a
withDB p a = liftIO $ withResource p (liftIO . a)

emailToLogin :: Text -> Text
emailToLogin = toLower
