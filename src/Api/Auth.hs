-- {-# LANGUAGE DataKinds         #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeOperators     #-}
-- {-# LANGUAGE DeriveGeneric #-}

module Api.Auth where

-- import           Control.Monad.Logger        (logDebugNS)
-- import           Servant
-- import           Data.Text                   (Text, pack)
-- import           Data.Text.Encoding
-- import Data.Maybe
-- import           Control.Monad.Except        (MonadIO)
-- import           Config                      (AppT (..), configOauth)
-- import           Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
-- import Data.Aeson           (ToJSON, toJSON, toEncoding, genericToEncoding, defaultOptions, object, (.=))
-- import GHC.Generics
-- import           Data.ByteString


-- data Authorized = AuthorizedFailed
--                 | AuthorizedSuccess Text String

-- data Authorize = Authorize {
--         url :: Text
--   } deriving (Generic, Show)

-- instance ToJSON Authorize where
--     -- No need to provide a toJSON implementation.

--     -- For efficiency, we write a simple toEncoding implementation, as
--     -- the default version uses toJSON.
--     toEncoding = genericToEncoding defaultOptions

-- instance ToJSON Authorized where
--     toJSON AuthorizedFailed =
--       object ["authorization" .= ("failed" :: Text)]
--     toJSON (AuthorizedSuccess s t) =
--       object ["authorization" .= ("succeed" :: Text)
--              ,"text" .= s
--              ,"string" .= t
--              ]

-- type AuthAPI =
--     "authorize" :> Get '[JSON] Authorize
--     :<|> "authorized" :> QueryParam "code" Text :> QueryParam "state" Text :> Get '[JSON] Authorized


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

-- authApi :: Proxy AuthAPI
-- authApi = Proxy

-- -- | The server that runs the UserAPI
-- authServer :: MonadIO m => ServerT AuthAPI (AppT m)
-- authServer = redirectAuthorize :<|> requestAuthorized

-- redirectAuthorize :: MonadIO m => AppT m (Authorize)
-- redirectAuthorize = do
--   authState <- asks configOauthState
--   oa <- asks configOauth
--   authorizeUrl <- getAuthorize authState oa mempty
--   throwError $ err301 { errHeaders = [("Location", Data.Text.Encoding.encodeUtf8 authorizeUrl)] }

-- requestAuthorized :: MonadIO m => Maybe Text -> Maybe Text -> AppT m (Authorized)
-- requestAuthorized mc ms = do
--   authState <- asks configOauthState
--   oa <- asks configOauth
--   mtoken <- getAuthorized oa authState mc ms
--   case mtoken of
--         Just (token,_) -> pure $ AuthorizedSuccess token (show ms)
--         Nothing    -> pure $ AuthorizedFailed
