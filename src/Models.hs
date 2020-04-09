{-# LANGUAGE DeriveGeneric              #-}

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Models where

import Model.CustomTypes()
import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool, entityKey, fromSqlKey)
import Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                              share, sqlSettings)
import Elm.Derive   (defaultOptions, deriveElmDef)

import Config               (Config, configPool)
import Data.Text            (Text)
-- import Data.Aeson           (FromJSON, ToJSON)
-- import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name Text
    email Text
    password Text
    UniqueEmail email
    deriving Show Eq
|]

-- data User = User
--   { userUid :: Text
--   , userPassword :: Text
--   , userEmail :: Text
--   } deriving (Generic)

-- duserToUser :: DUser -> User
-- duserToUser duser =
--   User {
--     userUid = fromSqlKey . entityKey $ duser :: Text
--   }

-- instance FromJSON User
-- instance ToJSON User

deriveElmDef defaultOptions ''User

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks configPool
    liftIO $ runSqlPool query pool
