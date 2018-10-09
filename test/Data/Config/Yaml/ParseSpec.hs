{-# LANGUAGE OverloadedStrings #-}

module Data.Config.Yaml.ParseSpec where

import Test.Hspec
import qualified Data.ByteString as B
import Data.Config.Structure
import Data.Config.Yaml.Parse

norm :: Structure -> Structure
norm (ScalarStruct t _) = ScalarStruct t ""
norm (ListStruct s) = ListStruct (norm s)
norm (MapStruct s) = MapStruct (norm s)
norm (DataStruct n ss) = DataStruct n $ map (\(k, s) -> (k, norm s)) ss

spec :: Spec
spec = do
    describe "Parse YAML" $ do
        it "from file" $ do
            DataStruct "Test" children <- parseYamlFile "data/test.yml" "Test"

            let child i = case (children !! i) of (n, s) -> (n, norm s)

            child 0 `shouldBe` ("f1", ScalarStruct StrElement "")
            child 1 `shouldBe` ("f2", ScalarStruct IntElement "")
            child 2 `shouldBe` ("f3", ScalarStruct DoubleElement "")
            child 3 `shouldBe` ("f4", ScalarStruct StrElement "")
            child 4 `shouldBe` ("f5", ScalarStruct StrElement "")
            child 5 `shouldBe` ("f6", ListStruct (ScalarStruct IntElement ""))
            child 6 `shouldBe` ("f7", ScalarStruct (NullableElement StrElement) "")
            child 7 `shouldBe` ("f8", ScalarStruct (NullableElement StrElement) "")
            child 8 `shouldBe` ("f9", ScalarStruct BoolElement "")
            child 9 `shouldBe` ("f10", ScalarStruct (NullableElement IntElement) "")
            child 10 `shouldBe` ("f11", ScalarStruct (UserElement "foo") "")
            child 11 `shouldBe` ("f12", DataStruct "f12" [ ("f12_1", ScalarStruct StrElement "")
                                                         , ("f12_2", ListStruct (ScalarStruct StrElement ""))
                                                         , ("f12_3", ScalarStruct (NullableElement BoolElement) "")
                                                         , ("f12_4", DataStruct "f12_4" [ ("f12_4_1", ScalarStruct StrElement "")
                                                                                        , ("f12_4_2", ScalarStruct IntElement "")
                                                                                        ])
                                                         ])
            child 12 `shouldBe` ("f13", ListStruct (ScalarStruct IntElement ""))
            child 13 `shouldBe` ("f14", ListStruct (DataStruct "f14" [ ("f14_1_1", ScalarStruct (NullableElement StrElement) "")
                                                                     , ("f14_1_2", ScalarStruct IntElement "")
                                                                     ]))
            child 14 `shouldBe` ("f15", ScalarStruct StrElement "")
            child 15 `shouldBe` ("f16", ScalarStruct IntElement "")
            child 16 `shouldBe` ("f17", ListStruct (DataStruct "f17" [ ("f17_1_1", ScalarStruct (NullableElement IntElement) "")
                                                                     , ("f17_1_2", ScalarStruct IntElement "")
                                                                     ]))
            child 17 `shouldBe` ("f18", ListStruct (DataStruct "f18" [ ("f17_1_1", ScalarStruct (NullableElement IntElement) "")
                                                                     , ("f17_1_2", ScalarStruct IntElement "")
                                                                     ]))
            child 18 `shouldBe` ("f19", MapStruct (ScalarStruct StrElement ""))
            child 19 `shouldBe` ("f20", MapStruct (DataStruct "f20_1" [ ("f20_1_1", ScalarStruct IntElement "")
                                                                      , ("f20_1_2", ScalarStruct BoolElement "")
                                                                      ]))


{-
f1: abc
f2: 12
f3: 1.5
f4: "def"
f5: 'ghi'
f6: [1, jkl, 2.5]
f7: null
f8:
f9: true
f10: !!int
f11: !foo mno
f12:
  f12_1: pqr
  f12_2: [stu]
  f12_3: !!bool
  f12_4:
    f12_4_1: vwx
    f12_4_2: 15
f13:
  - 1
  - 2
f14:
  - f14_1_1: !!str
    f14_1_2: 1412
  - f14_2_1: 
    f14_2_2: 1422
f15: &f15 f16
*f15: 16
f17: &f17
  - f17_1_1: !!int
    f17_1_2: 1712
f18: *f17
f19: !!map
  f19_1: yz
  f19_2: 192
f20: !!map
  f20_1:
    f20_1_1: 2011
    f20_1_2: false
-}