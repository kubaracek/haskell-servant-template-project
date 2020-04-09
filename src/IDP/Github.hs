{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Github where
import           Data.Aeson
import           Types
import           Utils
import           Data.Bifunctor
import           Data.Hashable
import           Data.Text.Lazy       (Text)
import           GHC.Generics
import           Network.OAuth.OAuth2
import           URI.ByteString
import           URI.ByteString.QQ

import           Types               (configOauth)
import           Control.Monad.Reader (asks)
import           Control.Monad.IO.Class (liftIO)

data Github = Github deriving (Show, Generic)

instance Hashable Github

instance IDP Github

instance HasLabel Github

instance HasTokenReq Github where
  tokenReq _ mgr token = do
    gk <- asks configOauth
    liftIO $ fetchAccessToken mgr gk token

instance HasTokenRefreshReq Github where
  tokenRefreshReq _ mgr token = do
    gk <- asks configOauth
    liftIO $ refreshAccessToken mgr gk token

instance HasUserReq Github where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Github where
  authUri _ = do
    gk <- asks configOauth
    liftIO $ pure $ createCodeUri gk [("state", "Github.test-state-123")]

data GithubUser = GithubUser { name :: Text
                             , id   :: Integer
                             } deriving (Show, Generic)

instance FromJSON GithubUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://api.github.com/user|]

toLoginUser :: GithubUser -> LoginUser
toLoginUser guser = LoginUser { loginUserName = name guser }
