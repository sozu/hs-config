{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Data.Config
import Data.Validation
import Debug.Trace

$(yamlConfiguration "data/th.yml" "Configuration" [])

main :: IO ()
main = do
    r <- loadYamlFile @Configuration' "data/th.yml"
    case r of
        Left c -> do
            forM_ (errorsOf c) print
        Right config -> do
            print $ string config
            print $ integer config
            print $ double config
            print $ boolean config
            print $ intlist config

            print $ mapping config
            print $ listing config

            print $ st_string $ subType config
            print $ st_integer $ subType config
