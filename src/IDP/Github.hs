{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Github where
import           Data.Aeson
import           Data.Bifunctor
import           Data.Hashable
import           Data.Text.Lazy         (Text)
import           GHC.Generics
import           Network.OAuth.OAuth2
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Utils

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (asks)
import           Types                  (configOauth)

data Github = Github
    deriving (Show, Generic)

instance Hashable Github

instance IDP Github

instance HasLabel Github

instance HasTokenReq Github where
  tokenReq _ gk mgr token = do
    fetchAccessToken mgr gk token

instance HasTokenRefreshReq Github where
  tokenRefreshReq _ gk mgr token = do
    refreshAccessToken mgr gk token

instance HasUserReq Github where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Github where
  authUri _ = do
    gk <- asks configOauth
    liftIO $ pure $ createCodeUri gk [("state", "Github.test-state-123")]

data GithubUser = GithubUser
    { name :: Text
    , id   :: Integer
    }
    deriving (Show, Generic)

instance FromJSON GithubUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://api.github.com/user|]

toLoginUser :: GithubUser -> LoginUser
toLoginUser guser = LoginUser { loginUserName = name guser }
