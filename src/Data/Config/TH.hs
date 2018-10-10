{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Data.Config.TH where

import GHC.Generics hiding (NoSourceUnpackedness, NoSourceStrictness)
import Language.Haskell.TH
import Control.Applicative
import Control.Monad
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.Maybe (maybe)
import Data.Aeson
import Data.Config.Structure

-- | Generates declarations of types contained in the structure.
--
-- Every @DataStruct@ in the structure generates a data type.
-- The name of each type is determined by appending its own name to the name of parent type with separator "'".
-- For example, if a @DataStruct@ whose name is "A" contains another @DataStruct@ whose name is "B",
-- the name of the inner @DataStruct@ is declared as "A'B".
defineDataTypes :: [String] -- ^ Path containing names of parent data types.
                -> [(String, Name)] -- ^ Mapping from tag string to user-defined type name.
                -> Structure -- ^ @Structure@.
                -> Q [Dec] -- ^ Declarations.
defineDataTypes p _ (ScalarStruct _ _)   = return []
defineDataTypes p m (ListStruct s)       = defineDataTypes p m s
defineDataTypes p m (MapStruct s)        = defineDataTypes p m s
defineDataTypes p m (DataStruct name fs) = (++) <$> (concat <$> mapM (defineDataTypes newPath m . snd) fs) <*> do
    typeName <- makeTypeName newPath
    let records = (`map` fs) $ \(n, s) -> do
            key <- makeRecordKey n
            (key, Bang NoSourceUnpackedness NoSourceStrictness,) <$> typeOf newPath (M.fromList m) s
    d <- dataD (cxt []) typeName [] Nothing [recC typeName records] [derivClause Nothing [conT ''Show, conT ''Generic]]
    i <- instanceD (cxt []) (appT (conT ''FromJSON) (conT typeName)) []
    return [d, i]
    where
        newPath = append name p

append :: a
       -> [a]
       -> [a]
append v vs = reverse $ v : reverse vs

makeRecordKey :: B.ByteString
              -> Q Name
makeRecordKey n = do
    let ident = UTF8.toString n
    if isVarHead (head ident) && all isVarChar (tail ident)
        then return $ mkName ident
        else fail $ "'" ++ ident ++ "' is an invalid variable identifier"
    where
        isVarHead c = C.isLower c || c == '_' || C.generalCategory c == C.OtherLetter
        isVarChar c = C.isLower c || C.isUpper c || C.isDigit c || c `elem` "_'" || C.generalCategory c == C.OtherLetter

makeTypeName :: [String]
             -> Q Name
makeTypeName ns = do
    let ident = L.intercalate "'" ns
    if isConHead (head ident) && all isConChar (tail ident)
        then return $ mkName ident
        else fail $ "'" ++ ident ++ "' is an invalid constructor identifier"
    where
        isConHead c = C.isUpper c
        isConChar c = C.isLower c || C.isUpper c || C.isDigit c || c `elem` "_'" || C.generalCategory c == C.OtherLetter

typeOf :: [String]
       -> M.Map String Name -- ^ Mapping from tag string to user-defined type name.
       -> Structure
       -> Q Type
typeOf p m s
    | ScalarStruct e _ <- s  = elementType e
    | ListStruct es <- s     = appT listT $ typeOf p m es
    | MapStruct es <- s      = appT (appT (conT ''M.Map) (conT ''String)) $ typeOf p m es
    | DataStruct name _ <- s = (makeTypeName $ append name p) >>= conT
    where
        lookupTypeName :: String -> Name
        lookupTypeName n = maybe (mkName n) id (m M.!? n)

        elementType :: ElementType -> Q Type
        elementType StrElement = conT ''String
        elementType IntElement = conT ''Integer
        elementType DoubleElement = conT ''Double
        elementType BoolElement = conT ''Bool
        elementType (NullableElement e) = appT (conT ''Maybe) (elementType e)
        elementType (UserElement n) = conT (lookupTypeName n)

