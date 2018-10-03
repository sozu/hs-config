{-# LANGUAGE OverloadedStrings #-}

module Data.Config.Yaml.Parse where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Loops
import Control.Monad.Trans.Resource
import Control.Exception.Safe
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Text.Read (readMaybe)
import Data.Maybe (maybe, isJust) 
import Data.Conduit 
import qualified Data.Conduit.List as CL
import Data.Yaml hiding (decodeFile)
import Text.Libyaml
import Data.Aeson
import Data.Config.Structure

-- | Exception type which shows reason of parsing failure.
--
-- Most of failures caused by invalid YAML format is represented with @ParserFailure@
-- which encloses @ParseException@ provided by @Data.Yaml@ package.
data YamlStructureException = ParserFailure ParseException
                            | UnexpectedEOF
                            | InvalidEvent String Event
                            | InvalidKeyAlias String
                            | NonMapDocument
                            deriving (Show)

instance Exception YamlStructureException

-- | Parses YAML formatted file and returns its structure.
--
-- The root element of the file have to be mapping.
-- @YamlStructureException@ is thrown when invalid format is found.
parseYamlFile :: FilePath
              -> String
              -> IO Structure
parseYamlFile path name = do
    (s, _) <- runResourceT $ (`runStateT` M.empty) $ runConduit $ decodeFile path .| parseYaml name
    return s

type Aliases = M.Map String Structure

type WithAliases m = StateT Aliases m

-- | Returns next event if exists, otherwise fails.
requireNext :: (MonadThrow m)
            => ConduitM Event o m Event
requireNext = do
    evt <- CL.head
    case evt of
        Just e -> return e
        Nothing -> throw UnexpectedEOF

-- | Checks if next event exists and fulfills a condition. Exception is throws if it doesn't.
checkNext :: (MonadThrow m)
          => (Event -> Bool)
          -> String
          -> ConduitM Event o m ()
checkNext check spec = do
    evt <- requireNext
    if check evt
        then return ()
        else throw $ InvalidEvent spec evt

-- | Looks up alias for an anchor name. If no alias found, this function throws an Exception.
lookupAnchor :: (MonadThrow m)
             => String
             -> WithAliases m Structure
lookupAnchor a = do
    aliases <- get
    case aliases M.!? a of
        Just s -> return s
        Nothing -> throw $ ParserFailure $ UnknownAlias a

-- | Looks up alias of string for an anchor name. If no alias found or the alias doesn't indicate a scalar, this function throws an Exception.
lookupString :: (MonadThrow m)
             => String
             -> WithAliases m B.ByteString
lookupString a = lookupAnchor a >>= \s -> do
    case s of
        ScalarStruct _ v -> return v
        _ -> throw $ InvalidKeyAlias a

-- Stores an alias to a structure with given anchor name if it exists.
putAnchor :: (MonadThrow m)
          => Maybe String
          -> Structure
          -> WithAliases m ()
putAnchor (Just a) s = modify $ M.insert a s
putAnchor Nothing _ = return ()

nextString :: (MonadThrow m)
           => ConduitM Event o (WithAliases m) (Either Event B.ByteString)
nextString = do
    evt <- requireNext
    case evt of
        (EventScalar v _ _ a) -> do
            lift $ putAnchor a (ScalarStruct StrElement v)
            return $ Right v
        (EventAlias a) -> do
            Right <$> lift (lookupString a)
        _ -> return $ Left evt

parseYaml :: (MonadThrow m)
          => String
          -> ConduitM Event o (WithAliases m) Structure
parseYaml name = do
    checkNext (== EventStreamStart) "Start of stream is expected"
    checkNext (== EventDocumentStart) "Start of document is expected"
    s <- parseDocument name
    checkNext (== EventDocumentEnd) "End of document is expected"
    checkNext (== EventStreamEnd) "End of stream is expected"
    return s

parseDocument :: (MonadThrow m)
              => String
              -> ConduitM Event o (WithAliases m) Structure
parseDocument name = do
    event <- requireNext
    case event of
        EventMappingStart tag style anchor -> parseMapping name tag anchor []
        _ -> throw NonMapDocument

parseObject :: (MonadThrow m)
            => String
            -> Event
            -> ConduitM Event o (WithAliases m) Structure
parseObject _ (EventScalar v t s a) = do
    let struct = ScalarStruct (parseScalar v t s) v
    lift $ putAnchor a struct
    return struct
parseObject n (EventMappingStart t' s' a') = parseMapping n t' a' []
parseObject n (EventSequenceStart t' s' a') = parseSequence n t' a' []
parseObject _ (EventAlias a') = lift $ lookupAnchor a'

-- | Interpret a scalar event into an element type.
parseScalar :: B.ByteString
            -> Tag
            -> Style
            -> ElementType
parseScalar v NoTag s
    -- When an element doesn't have a tag, the type is determined by its style.
    | s == SingleQuoted = StrElement
    | s == DoubleQuoted = StrElement
    | s == Literal      = StrElement
    | s == Folded       = StrElement
    -- When the style is not determinant, the type is infered by the value.
    | isNullScalar v    = NullableElement StrElement
    | isBool v          = BoolElement
    | isIntegral v      = IntElement
    | isDouble v        = DoubleElement
    | otherwise         = StrElement
    where
        toLower = C8.map C.toLower
        isBool v = toLower v `elem` ["y", "yes", "on", "true", "n", "no", "off", "false"]
        isIntegral v = isJust (readMaybe (C8.unpack v) :: Maybe Integer)
        isDouble v = isJust (readMaybe (C8.unpack v) :: Maybe Double)
parseScalar v NullTag s  = NullableElement (parseScalar v NoTag s)
parseScalar v StrTag _   = applyNullable v StrElement
parseScalar v IntTag _   = applyNullable v IntElement
parseScalar v FloatTag _ = applyNullable v DoubleElement
parseScalar v BoolTag _  = applyNullable v BoolElement
-- Tags which are not available in scalar context are just ignored.
parseScalar v SetTag s   = parseScalar v NoTag s
parseScalar v SeqTag s   = parseScalar v NoTag s
parseScalar v MapTag s   = parseScalar v NoTag s
-- User defined type.
parseScalar _ (UriTag uri) _ = UserElement $ reverse $ takeWhile (/= ':') (reverse uri)

-- | Checks whether the string represents null.
isNullScalar :: B.ByteString
             -> Bool
isNullScalar v = C8.map C.toLower v `elem` ["", "~", "null"]

-- | Wrap an element type with @NullableElement@ if the string represents null.
applyNullable :: B.ByteString
              -> ElementType
              -> ElementType
applyNullable v et = if isNullScalar v then NullableElement et else et

-- | Parses mapping element and collects them as a @DataStruct@.
--
-- When '!map' tag is explicitly given, the type of this element is considered as @Map String a@
-- where @a@ is determined by the type of first item.
parseMapping :: (MonadThrow m)
             => String
             -> Tag
             -> Anchor
             -> [(B.ByteString, Structure)]
             -> ConduitM Event o (WithAliases m) Structure
parseMapping _ MapTag anchor _ = do
    -- TODO: Allows value types other than string.
    key <- nextString
    case key of
        Left EventMappingEnd -> do
            let s = MapStruct (ScalarStruct StrElement "")
            lift $ putAnchor anchor s
            return s
        Left e -> throw $ InvalidEvent "Scalar or the end of map is expected" e
        Right k -> do
            value <- requireNext >>= parseObject (C8.unpack k)
            let s = MapStruct value
            lift $ putAnchor anchor s
            iterateWhile (/= EventMappingEnd) requireNext
            return s
parseMapping name tag anchor structures = do
    key <- nextString
    case key of
        Left EventMappingEnd -> do
            let s = DataStruct name $ reverse structures
            lift $ putAnchor anchor s
            return s
        Left e -> throw $ InvalidEvent "Scalar or the end of map is expected" e
        Right k -> do
            value <- requireNext >>= parseObject (C8.unpack k)
            parseMapping name tag anchor $ (k, value) : structures

-- | Interprets sequence element as a @ListStruct@.
--
-- When the sequence has items, the first one determines the type of list item. Otherwise, @String@ is selected.
parseSequence :: (MonadThrow m)
              => String
              -> Tag
              -> Anchor
              -> [Structure]
              -> ConduitM Event o (WithAliases m) Structure
parseSequence name tag anchor structures = do
    evt <- requireNext
    case evt of
        EventSequenceEnd -> return $ ListStruct (ScalarStruct StrElement "")
        _ -> do
            s <- ListStruct <$> parseObject name evt
            iterateWhile (/= EventSequenceEnd) requireNext
            lift $ putAnchor anchor s
            return s