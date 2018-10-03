module Data.Config (
    module Data.Config.Structure
  , module Data.Config.TH
  , module Data.Config.Yaml.Parse
  , yamlConfiguration
) where

import Language.Haskell.TH
import Data.Config.Structure
import Data.Config.TH
import Data.Config.Yaml.Parse

-- | Generates data types which represent the contents of YAML file.
yamlConfiguration :: FilePath -- ^ Path to YAML.
                  -> String -- ^ Name of the generated type.
                  -> [(String, Name)] -- ^ Mapping from tag string to user-defined type name.
                  -> Q [Dec] -- ^ Declarations of data types.
yamlConfiguration yaml n m = do
    s <- runIO $ parseYamlFile yaml n
    defineDataTypes [] m s
