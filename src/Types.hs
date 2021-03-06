{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module Types where

import           Control.Concurrent.MVar
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8        as BSL
import           Data.Hashable
import qualified Data.HashMap.Strict               as Map
import           Data.Text.Lazy
import qualified Data.Text.Lazy                    as TL
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import           Control.Monad.Reader (ReaderT)
import           Database.Persist.Postgresql          (ConnectionPool)
import           Control.Monad.Metrics                (Metrics)
import           Control.Concurrent                   (ThreadId)
import           Network.Wai.Handler.Warp             (Port)
import           Logger

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

-- | The Config for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Config
    = Config
    { configPool       :: ConnectionPool
    , configEnv        :: Environment
    , configMetrics    :: Metrics
    , configEkgServer  :: ThreadId
    , configLogEnv     :: LogEnv
    , configPort       :: Port
    , configOauth      :: OAuth2
    , configCache      :: CacheStore
    }

type IDPLabel = Text

-- TODO: how to make following type work??
-- type CacheStore = forall a. IDP a => MVar (Map.HashMap a IDPData)
type CacheStore = MVar (Map.HashMap IDPLabel IDPData)

-- * type class for defining a IDP
--
class (Hashable a, Show a) => IDP a

class (IDP a) => HasLabel a where
  idpLabel :: a -> IDPLabel
  idpLabel = TL.pack . show

class (IDP a) => HasAuthUri a where
  authUri :: a -> ReaderT Config IO (Text)

class (IDP a) => HasTokenReq a where
  tokenReq :: a -> OAuth2 -> Manager -> ExchangeToken -> IO (OAuth2Result TR.Errors OAuth2Token)

class (IDP a) => HasTokenRefreshReq a where
  tokenRefreshReq :: a -> OAuth2 -> Manager -> RefreshToken -> IO (OAuth2Result TR.Errors OAuth2Token)

class (IDP a) => HasUserReq a where
  userReq :: a -> Manager -> AccessToken -> IO (Either BSL.ByteString LoginUser)

-- Heterogenous collections
-- https://wiki.haskell.org/Heterogenous_collections
--
data IDPApp = forall a. (IDP a,
                         HasTokenRefreshReq a,
                         HasTokenReq a,
                         HasUserReq a,
                         HasLabel a,
                         HasAuthUri a) => IDPApp a

-- dummy oauth2 request error
--
data Errors =
  SomeRandomError
  deriving (Show, Eq, Generic)

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }

newtype LoginUser =
  LoginUser { loginUserName :: Text
            } deriving (Eq, Show)

data IDPData =
  IDPData { codeFlowUri     :: Text
          , loginUser       :: Maybe LoginUser
          , oauth2Token     :: Maybe OAuth2Token
          , idpDisplayLabel :: IDPLabel
          }

-- simplify use case to only allow one idp instance for now.
instance Eq IDPData where
  a == b = idpDisplayLabel a == idpDisplayLabel b

instance Ord IDPData where
  a `compare` b = idpDisplayLabel a `compare` idpDisplayLabel b
