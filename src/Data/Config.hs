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
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Yaml
import Data.Config.Structure
import Data.Config.TH
import Data.Config.Yaml.Parse
import Data.Validation
import Data.Validation.TH

-- | Generates data types which represent the contents of YAML file.
--
-- The root element of the yaml file should be mapping, otherwise this function throws an error.
-- Each item of the mapping is represented as a record of declared data type
-- whose name and type is determined by the key and value of the item respectively.
--
-- In addition to data types defined by YAML structure, this function also generates their validatable versions.
-- Among them, the validatable type of root element whose name is determined by appending single quote to the second argument
-- is designed to be used on invocation of @loadYamlFile@.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > {-# LANGUAGE TypeApplications #-}
-- >
-- > import Data.Config
-- > import Data.Validation
-- >
-- > $(yamlConfiguration "path/to/template.yaml" "Configuration" [])
-- > -- Above function generates a type (Configuration) and its valiadable version (Configuration').
-- > -- data Configuration = ...
-- > -- data Configuration' = ...
-- >
-- > main = do
-- >   res <- loadYamlFile @Configuration' "path/to/config.yaml"
-- >   case res of
-- >     Left c' -> ... -- validation error, the type of c' is Configuration'.
-- >     Right c -> ... -- success, the type of c is Configuration.
yamlConfiguration :: FilePath -- ^ Path to YAML.
                  -> String -- ^ Name of the generated type.
                  -> [(String, Name)] -- ^ Mapping from tag string to user-defined type name.
                  -> Q [Dec] -- ^ Declarations of data types.
yamlConfiguration yaml n m = do
    s <- runIO $ parseYamlFile yaml n
    ds <- defineDataTypes [] m s 
    let datas = catMaybes $ map newDataName ds
    let names = map fst datas
    vs <- concat <$> mapM (\(n, c) -> declareValidatable names n c) datas
    return $ ds ++ vs
    where
        newDataName :: Dec -> Maybe (Name, Con)
        newDataName (DataD _ n _ _ (c:_) _) = Just (n, c)
        newDataName _ = Nothing

-- | Loads YAML file and validate its content.
--
-- @c@ should be specified explicitly with a validatable type declared by @yamlConfiguration@.
--
-- Errors on YAML parsing will be thrown as Exceptions.
-- On the other hand, validation failure is returned as Either monad. 
loadYamlFile :: forall c m. (Validatable c, FromJSON c, MonadIO m)
             => FilePath -- ^ Path to YAML.
             -> m (Either c (ValidationTarget c)) -- ^ Validation result.
loadYamlFile path = do
    config' <- decodeFileThrow @m @c path
    return $ case validate config' of
        Just config -> Right config
        Nothing     -> Left config'