{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Config (
    module Data.Config.Structure
  , module Data.Config.TH
  , module Data.Config.Yaml.Parse
  , yamlConfiguration
  , loadYamlFile
) where

import Language.Haskell.TH
import Control.Monad.IO.Class
import Data.Aeson
import Data.Yaml
import Data.Config.Structure
import Data.Config.TH
import Data.Config.Yaml.Parse
import Data.Validation

-- | Generates data types which represent the contents of YAML file.
yamlConfiguration :: FilePath -- ^ Path to YAML.
                  -> String -- ^ Name of the generated type.
                  -> [(String, Name)] -- ^ Mapping from tag string to user-defined type name.
                  -> Q [Dec] -- ^ Declarations of data types.
yamlConfiguration yaml n m = do
    s <- runIO $ parseYamlFile yaml n
    defineDataTypes [] m s 

loadYamlFile :: forall c m. (Validatable c, FromJSON c, MonadIO m)
             => FilePath
             -> m (Either c (ValidationTarget c))
loadYamlFile path = do
    config' <- decodeFileThrow @m @c path
    return $ case validate config' of
        Just config -> Right config
        Nothing     -> Left config'