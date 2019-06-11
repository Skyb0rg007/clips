-- | CLIPS types
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE TypeFamilies             #-}

module Clips.Types (
      Environment (..)
    , Fact (..)
    , Instance (..)
    , Clips (..)
    , ClipsVal (..)
    , TypeHeader (..)
    ) where

import           Data.String     (IsString (..))
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Foreign
import           Foreign.C.Types
import           GHC.Exts        (IsList (..))

#include "clips.h"

newtype Environment = Environment (Ptr Environment)
    deriving (Eq, Ord)

instance Show Environment where
    show (Environment ptr) = "Env: " ++ show ptr

newtype Fact = Fact (ForeignPtr Fact)
    deriving (Eq, Ord)

instance Show Fact where
    show (Fact p) = "Fact " ++ show p

newtype Instance = Instance (ForeignPtr Instance)
    deriving (Eq, Ord)

instance Show Instance where
    show (Instance p) = "Instance " ++ show p

data ClipsVal
    = ClipsVoid
    | ClipsPointer    !(Ptr ())
    | ClipsFloat      !Double
    | ClipsInteger    !Int
    | ClipsSymbol     !Text
    | ClipsString     !Text
    | ClipsMultifield ![ClipsVal]
    | ClipsFact       !Fact
    | ClipsInstance   !Instance
    deriving (Eq)

instance Show ClipsVal where
    show (ClipsVoid)          = "*void*"
    show (ClipsPointer p)     = show p
    show (ClipsFloat f)       = show f
    show (ClipsInteger i)     = show i
    show (ClipsSymbol s)      = "'" ++ T.unpack s ++ "'"
    show (ClipsString s)      = show s
    show (ClipsFact f)        = show f
    show (ClipsInstance i)    = show i
    show (ClipsMultifield xs) = show xs

instance Num ClipsVal where
    (ClipsInteger a) + (ClipsInteger b) = ClipsInteger $ a + b
    a + b = error $ "Unable to add ClipsValues " ++ show a ++ " and " ++ show b
    (ClipsInteger a) - (ClipsInteger b) = ClipsInteger $ a + b
    a - b = error $ "Unable to subtract ClipsValues " ++ show a ++ " and " ++ show b
    (ClipsInteger a) * (ClipsInteger b) = ClipsInteger $ a + b
    a * b = error $ "Unable to multiply ClipsValues " ++ show a ++ " and " ++ show b
    abs (ClipsInteger i) = ClipsInteger (abs i)
    abs a = error $ "Unable to make absolute value of " ++ show a
    signum (ClipsInteger i) = ClipsInteger (signum i)
    signum a = error $ "Unable to take signum of " ++ show a
    fromInteger = ClipsInteger . fromInteger

instance IsString ClipsVal where
    fromString = ClipsString . fromString

instance IsList ClipsVal where
    type Item ClipsVal = ClipsVal
    fromList = ClipsMultifield
    toList (ClipsMultifield xs) = xs
    toList x = pure x

class Clips a where
    toClipsVal :: a -> ClipsVal
    fromClipsVal :: ClipsVal -> Maybe a

instance Clips Int where
    toClipsVal = ClipsInteger
    fromClipsVal (ClipsInteger i) = Just i
    fromClipsVal _                = Nothing

instance Clips Double where
    toClipsVal = ClipsFloat
    fromClipsVal (ClipsFloat f) = Just f
    fromClipsVal _              = Nothing

instance Clips Fact where
    toClipsVal = ClipsFact
    fromClipsVal (ClipsFact f) = Just f
    fromClipsVal _             = Nothing

instance Clips Instance where
    toClipsVal = ClipsInstance
    fromClipsVal (ClipsInstance i) = Just i
    fromClipsVal _                 = Nothing

{# enum define TypeHeader
    { EXTERNAL_ADDRESS_TYPE as TH_EXTERNAL_ADDRESS_TYPE
    , VOID_TYPE             as TH_VOID_TYPE
    , SYMBOL_TYPE           as TH_SYMBOL_TYPE
    , STRING_TYPE           as TH_STRING_TYPE
    , MULTIFIELD_TYPE       as TH_MULTIFIELD_TYPE
    , INTEGER_TYPE          as TH_INTEGER_TYPE
    , INSTANCE_NAME_TYPE    as TH_INSTANCE_NAME_TYPE
    , INSTANCE_ADDRESS_TYPE as TH_INSTANCE_ADDRESS_TYPE
    , FLOAT_TYPE            as TH_FLOAT_TYPE
    , FACT_ADDRESS_TYPE     as TH_FACT_ADDRESS_TYPE
    } deriving (Show, Eq)
    #}

instance Storable TypeHeader where
    sizeOf _ = {# sizeof TypeHeader #}
    alignment _ = {# alignof TypeHeader #}
    peek = fmap fromUShort . peek . castPtr
      where
        fromUShort :: CUShort -> TypeHeader
        fromUShort = toEnum . fromIntegral
    poke ptr = poke (castPtr ptr) . toUShort
      where
        toUShort :: TypeHeader -> CUShort
        toUShort = toEnum . fromEnum

