{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Auth where

import           Control.Monad.Logger        (logDebugNS)
import           Data.Bifunctor

import           Network.HTTP.Conduit        (newManager, tlsManagerSettings, Manager)
import           Servant
import           Data.Text                   (Text, pack)
import           Data.Text.Encoding
import Data.Maybe
import           Data.Text
import qualified Data.Text.Lazy              as TL
import           Data.Text.Lazy.Encoding     (encodeUtf8)
import           Control.Monad               (when)
import           Control.Monad.Except        (MonadIO)
import           Config                      (AppT (..))
import           Types                       (configCache)
import           Session                     (allValues, lookupKey, insertIDPData)
import           Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO, lift, runReaderT)
import Data.Aeson           (ToJSON, toJSON, toEncoding, genericToEncoding, defaultOptions, object, (.=))
import GHC.Generics
import           Data.ByteString
import Types
import Utils     (bslToText)
import IDP       (parseIDP)
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
    "list-idps" :> Get '[JSON] [OAuthIDP]
    :<|> "authorized" :> QueryParam "code" TL.Text :> QueryParam "state" TL.Text :> Get '[JSON] Authorized


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
authServer = listIDPs :<|> authorizedCallback

data OAuthIDP =
  OAuthIDP { label :: IDPLabel
           , authUrl   :: TL.Text
           } deriving (Generic)

instance ToJSON OAuthIDP

listIDPs :: MonadIO m => AppT m ([OAuthIDP])
listIDPs = do
  cache <- asks configCache
  idps <- liftIO $ allValues cache
  pure $ fmap (\x -> OAuthIDP (idpDisplayLabel x) (codeFlowUri x)) idps

authorizedCallback :: MonadIO m => Maybe TL.Text -> Maybe TL.Text -> AppT m (Authorized)
authorizedCallback mc ms = do
  case (mc, ms) of
    (Just code, Just state) -> do
      cache <- asks configCache
      let eitherIdpApp = parseIDP (TL.takeWhile (/= '.') state)
      case eitherIdpApp of
        Right (IDPApp idp) -> fetchTokenAndUser cache code idp
        Left _ -> throwError err400

    _ -> throwError err400
  where
    lookIdp :: (MonadIO m, HasLabel a) =>
           CacheStore -> a -> m (Maybe IDPData)
    lookIdp c1 idp1 = liftIO $ lookupKey c1 (idpLabel idp1)

    fetchTokenAndUser :: (MonadIO m, HasTokenReq a, HasUserReq a, HasLabel a)
                  => CacheStore
                  -> TL.Text           -- ^ code
                  -> a
                  -> AppT m (Authorized)
    fetchTokenAndUser cache code idp = do
        maybeIdpData <- lookIdp cache idp
        when (isNothing maybeIdpData) (throwError err400 { errBody = "Cannot find data in cache"})
        let idpData = fromJust maybeIdpData
        result <- fetchTokenAndUser' cache (TL.toStrict code) idp idpData
        case result of
                Right _  -> return $ AuthorizedSuccess "some" "body"
                Left err -> throwError err400 { errBody = Data.Text.Lazy.Encoding.encodeUtf8 $ err}
               
    fetchTokenAndUser' :: (MonadIO m, HasTokenReq a, HasUserReq a) =>
                CacheStore -> Text -> a -> IDPData -> AppT m (Either TL.Text ())
    fetchTokenAndUser' c code idp idpData = do
        githubKey <- asks configOauth
        mgr <- liftIO $ newManager tlsManagerSettings
        token <- liftIO $ tokenReq idp githubKey mgr (ExchangeToken $ code)

        result <- case token of
                Right at -> tryFetchUser mgr at idp
                Left e   -> return (Left $ TL.pack $ "tryFetchUser: cannot fetch asses token. error detail: " ++ show e)

        case result of
                Right (luser, at) -> liftIO $ updateIdp c idpData luser at >> return (Right ())
                Left err    -> return $ Left ("fetchTokenAndUser: " `TL.append` err)

        where
          updateIdp c1 oldIdpData luser token =
                insertIDPData c1 (oldIdpData {loginUser = Just luser, oauth2Token = Just token })

          tryFetchUser :: (MonadIO m, HasUserReq a) =>
                              Manager
                              -> OAuth2Token -> a -> AppT m (Either TL.Text (LoginUser, OAuth2Token))
          tryFetchUser mgr at idp = do
                re <- fetchUser idp mgr (accessToken at)
                return $ case re of
                        Right user' -> Right (user', at)
                        Left e      -> Left e

                        -- * Fetch UserInfo
                        --
          fetchUser :: (MonadIO m, HasUserReq a) => a -> Manager -> AccessToken -> AppT m (Either TL.Text LoginUser)
          fetchUser idp mgr token = do
                        re <- liftIO $ userReq idp mgr token
                        return (first bslToText re)
