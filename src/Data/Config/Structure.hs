module Data.Config.Structure where

import qualified Data.ByteString as B

-- | Data type of scalar value.
data ElementType = StrElement -- ^ String.
                 | IntElement -- ^ Integral number.
                 | DoubleElement -- ^ Real number.
                 | BoolElement -- ^ Boolean.
                 | NullableElement ElementType -- ^ Value of @ElementType@ which can be null.
                 | UserElement String -- ^ Value of user defined type.
                 deriving (Show, Eq)

-- | Structure representing the data type of values in key-value representation of configuration data.
data Structure = ScalarStruct ElementType B.ByteString -- ^ Scalar value of the @ElementType@ and original @B.ByteString@.
               | ListStruct Structure -- ^ List of the data type represented with the @Structure@.
               | MapStruct Structure -- ^ Map having string key and value represented with the @Structure@.
               | DataStruct String [(B.ByteString, Structure)] -- ^ New data type with its name and records.
               deriving (Show, Eq)
