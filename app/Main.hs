{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Config
import Data.Yaml
import Debug.Trace

yamlConfiguration "data/th.yml" "Configuration" []

main :: IO ()
main = do
    --structure <- parseYamlFile "data/test.yml" "Test"
    --print structure

    config <- decodeFileThrow @_ @Configuration "data/th.yml" 

    print $ string config
    print $ integer config
    print $ double config
    print $ boolean config
    print $ intlist config

    print $ mapping config
    print $ listing config

    print $ st_string $ subType config
    print $ st_integer $ subType config
