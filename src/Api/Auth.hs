{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Auth where

import           Control.Monad.Logger        (logDebugNS)
import           Servant
import           Data.Text                   (Text, pack)
import           Data.Text.Encoding
import Data.Maybe
import qualified Data.Text.Lazy                    as TL
import           Control.Monad.Except        (MonadIO)
import           Config                      (AppT (..))
import           Types                       (configCache)
import           Session                     (allValues)
import           Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Data.Aeson           (ToJSON, toJSON, toEncoding, genericToEncoding, defaultOptions, object, (.=))
import GHC.Generics
import           Data.ByteString
import Types
import           Network.OAuth.OAuth2


data Authorized = AuthorizedFailed
                | AuthorizedSuccess Text String

data Authorize = Authorize {
        url :: Text
  } deriving (Generic, Show)

instance ToJSON Authorize where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Authorized where
    toJSON AuthorizedFailed =
      object ["authorization" .= ("failed" :: Text)]
    toJSON (AuthorizedSuccess s t) =
      object ["authorization" .= ("succeed" :: Text)
             ,"text" .= s
             ,"string" .= t
             ]

type AuthAPI =
    "list-idps" :> Get '[JSON] [LoginUrl]
    -- :<|> "authorized" :> QueryParam "code" Text :> QueryParam "state" Text :> Get '[JSON] Authorized


-- doRefreshToken :: HasTokenRefreshReq a => a -> IDPData -> IO (Either String OAuth2Token)
-- doRefreshToken idp idpData = do
--   mgr <- newManager tlsManagerSettings
--   case oauth2Token idpData of
--     Nothing -> return $ Left "no token found for idp"
--     Just at ->
--       case refreshToken at of
--         Nothing -> return $ Left "no refresh token presents"
--         Just rt -> do
--           re <- tokenRefreshReq idp mgr rt
--           return (first show re)

authApi :: Proxy AuthAPI
authApi = Proxy

-- | The server that runs the UserAPI
authServer :: MonadIO m => ServerT AuthAPI (AppT m)
authServer = listIDPs -- :<|> requestAuthorized

data LoginUrl = LoginUrl TL.Text deriving (Generic)

instance ToJSON LoginUrl

listIDPs :: MonadIO m => AppT m ([LoginUrl])
listIDPs = do
  cache <- asks configCache
  idps <- liftIO $ allValues cache
  pure $ fmap (\x -> LoginUrl (codeFlowUri x)) idps

-- requestAuthorized :: MonadIO m => Maybe Text -> Maybe Text -> AppT m (Authorized)
-- requestAuthorized mc ms = do
--   authState <- asks configOauthState
--   oa <- asks configOauth
--   mtoken <- getAuthorized oa authState mc ms
--   case mtoken of
--         Just (token,_) -> pure $ AuthorizedSuccess token (show ms)
--         Nothing    -> pure $ AuthorizedFailed
