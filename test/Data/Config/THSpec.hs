{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Config.THSpec where

import Test.Hspec
import Language.Haskell.TH
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.UTF8 as UTF8
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
    describe "Check names of records" $ do
        it "Valid ascii name" $ do
            n <- runQ $ makeRecordKey "abc"
            nameBase n `shouldBe` "abc"
        it "Valid name including number" $ do
            n <- runQ $ makeRecordKey "abc1"
            nameBase n `shouldBe` "abc1"
        it "Valid name starting with underscore" $ do
            n <- runQ $ makeRecordKey "_abc_"
            nameBase n `shouldBe` "_abc_"
        it "Valid name including single quote" $ do
            n <- runQ $ makeRecordKey "abc'"
            nameBase n `shouldBe` "abc'"
        it "Valid name starting with multibyte character" $ do
            n <- runQ $ makeRecordKey $ UTF8.fromString "あabcあ"
            nameBase n `shouldBe` "あabcあ"
        it "Invalid name starting with capital letter" $ do
            runQ (makeRecordKey "Abc") `shouldThrow` anyException
        it "Invalid name starting with number" $ do
            runQ (makeRecordKey "1bc") `shouldThrow` anyException
        it "Invalid name starting with single quote" $ do
            runQ (makeRecordKey "'bc") `shouldThrow` anyException
        it "Invalid name including symbols" $ do
            runQ (makeRecordKey "abc#") `shouldThrow` anyException
        it "Invalid name including space" $ do
            runQ (makeRecordKey "abc ") `shouldThrow` anyException
        it "Invalid name including multi byte space" $ do
            runQ (makeRecordKey "abc　") `shouldThrow` anyException

    describe "Check names of types" $ do
        it "Valid ascii name" $ do
            n <- runQ $ makeTypeName ["Abc"]
            nameBase n `shouldBe` "Abc"
        it "Valid name including number" $ do
            n <- runQ $ makeTypeName ["Abc1"]
            nameBase n `shouldBe` "Abc1"
        it "Valid name including underscore" $ do
            n <- runQ $ makeTypeName ["Abc_"]
            nameBase n `shouldBe` "Abc_"
        it "Valid name including single quote" $ do
            n <- runQ $ makeTypeName ["Abc'"]
            nameBase n `shouldBe` "Abc'"
        it "Valid name including multibyte character" $ do
            n <- runQ $ makeTypeName ["Abcあ"]
            nameBase n `shouldBe` "Abcあ"
        it "Invalid name starting with small letter" $ do
            runQ (makeTypeName ["abc"]) `shouldThrow` anyException
        it "Invalid name starting with number" $ do
            runQ (makeTypeName ["1bc"]) `shouldThrow` anyException
        it "Invalid name starting with underscore" $ do
            runQ (makeTypeName ["_Abc"]) `shouldThrow` anyException
        it "Invalid name starting with single quote" $ do
            runQ (makeTypeName ["'Abc"]) `shouldThrow` anyException
        it "Invalid name starting with multibyte character" $ do
            runQ (makeTypeName ["あAbc"]) `shouldThrow` anyException
        it "Invalid name including symbols" $ do
            runQ (makeTypeName ["Abc#"]) `shouldThrow` anyException
        it "Invalid name including space" $ do
            runQ (makeTypeName ["Abc "]) `shouldThrow` anyException
        it "Invalid name including multi byte space" $ do
            runQ (makeTypeName ["Abc　"]) `shouldThrow` anyException

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