{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.CustomTypes where

import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.ByteString.Char8 as B8
import Database.Persist.Sql

instance PersistField UUID where
  toPersistValue = PersistDbSpecific . B8.pack . UUID.toString
  fromPersistValue (PersistDbSpecific uuid) =
    case UUID.fromString $ B8.unpack uuid of
      Nothing -> Left "Invalid UUID"
      Just uuid' -> Right uuid'
  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"
