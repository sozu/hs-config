{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Config.THSpec where

import Test.Hspec
import Language.Haskell.TH
import Control.Monad
import qualified Data.Map as M
import Data.Config

data UserDefined1

checkType :: Dec -> String -> [(String, Type)] -> Expectation
checkType (DataD _ n _ _ cs _) name records = do
    nameBase n `shouldBe` name
    forM_ cs $ \c -> do
        case c of
            RecC cn recs -> do
                nameBase cn `shouldBe` name
                length recs `shouldBe` length records
                forM_ (zip recs records) $ \((rn, _, rt), (n, t)) -> (nameBase rn, rt) `shouldBe` (n, t)
            _ -> expectationFailure $ "Unexpected data constructor: " ++ show c
checkType d _ _ = expectationFailure $ "Unexpected data type declaration: " ++ show d

spec :: Spec
spec = do
    describe "Generate data type from structure" $ do
        it "Basic data types" $ do
            ds <- runQ $ defineDataTypes [] [] $
                    DataStruct "Test" [ ("f1", ScalarStruct StrElement "abc")
                                      , ("f2", ScalarStruct IntElement "1")
                                      , ("f3", ScalarStruct DoubleElement "2.5")
                                      , ("f4", ScalarStruct BoolElement "true")
                                      , ("f5", ScalarStruct (NullableElement IntElement) "")
                                      ]
            checkType (ds !! 0) "Test" [ ("f1", ConT ''String)
                                       , ("f2", ConT ''Integer)
                                       , ("f3", ConT ''Double)
                                       , ("f4", ConT ''Bool)
                                       , ("f5", AppT (ConT ''Maybe) (ConT ''Integer))
                                       ]

        it "User defined types" $ do
            ds <- runQ $ defineDataTypes [] [("user", ''UserDefined1)] $
                    DataStruct "Test" [ ("f1", ScalarStruct (UserElement "user") "")
                                      , ("f2", ScalarStruct (UserElement "UserDefined2") "")
                                      ]
            checkType (ds !! 0) "Test" [ ("f1", ConT ''UserDefined1)
                                       , ("f2", ConT $ mkName "UserDefined2")
                                       ]

        it "Collection structure" $ do
            ds <- runQ $ defineDataTypes [] [] $
                    DataStruct "Test" [ ("f1", ListStruct (ScalarStruct IntElement "1"))
                                      , ("f2", MapStruct (ScalarStruct IntElement "2"))
                                      ]
            checkType (ds !! 0) "Test" [ ("f1", AppT ListT (ConT ''Integer))
                                       , ("f2", AppT (AppT (ConT ''M.Map) (ConT ''String)) (ConT ''Integer))
                                       ]

        it "Nested structure" $ do
            ds <- runQ $ defineDataTypes [] [] $
                    DataStruct "Test" [ ("f1", DataStruct "Nested" [("f2", ScalarStruct IntElement "1")])]
            length ds `shouldBe` 4
            checkType (ds !! 0) "Test'Nested" [ ("f2", ConT ''Integer) ]
            checkType (ds !! 2) "Test" [ ("f1", ConT $ mkName "Test'Nested") ]