module IDP where

import           Data.Text.Lazy      (Text)

import qualified Data.HashMap.Strict as Map
import qualified IDP.Github          as IGithub
import           Session
import           Types
import           Control.Monad.Reader (ReaderT)
import           Config
import           Control.Monad.IO.Class (liftIO)


-- TODO: make this generic to discover any IDPs from idp directory.
--
idps :: [IDPApp]
idps = [ IDPApp IGithub.Github
       ]

initIdps :: CacheStore -> ReaderT Config IO ()
initIdps c = do
  idpDatas <- mapM mkIDPData idps
  liftIO $ mapM_ (insertIDPData c) idpDatas

idpsMap :: Map.HashMap Text IDPApp
idpsMap = Map.fromList $ fmap (\x@(IDPApp idp) -> (idpLabel idp, x)) idps

parseIDP :: Text -> Either Text IDPApp
parseIDP s = maybe (Left s) Right (Map.lookup s idpsMap)

mkIDPData :: IDPApp -> ReaderT Config IO IDPData
mkIDPData (IDPApp idp) = do
  uri <- authUri idp
  pure $ IDPData uri Nothing Nothing (idpLabel idp)
