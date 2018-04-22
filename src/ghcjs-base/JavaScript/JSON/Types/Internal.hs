{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, EmptyDataDecls,
             DeriveDataTypeable, GHCForeignImportPrim, DataKinds, KindSignatures,
             PolyKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,
             UnboxedTuples, MagicHash, UnliftedFFITypes
  #-}
module JavaScript.JSON.Types.Internal
    ( -- * Core JSON types
      SomeValue(..),  Value  
    , SomeValue'(..), Value'
    , MutableValue, MutableValue'
    , emptyArray, isEmptyArray
    , Pair
    , Object, MutableObject
    , objectProperties, objectPropertiesIO
    , objectAssocs,     objectAssocsIO
    , Lookup(..), IOLookup(..)
    , emptyObject
    , match
    , arrayValue, stringValue, doubleValue, nullValue, boolValue, objectValue
    , arrayValueList, indexV
      {-  fixme implement freezing / thawing
    , freeze, unsafeFreeze
    , thaw,   unsafeThaw
       -}
      -- * Type conversion
    , Parser
    , Result(..)
    , parse
    , parseEither
    , parseMaybe
    , modifyFailure
    , encode
      -- * Constructors and accessors
    , object
    
      -- * Generic and TH encoding configuration
    , Options(..)
    , SumEncoding(..)
    , defaultOptions
    , defaultTaggedObject
    
      -- * Used for changing CamelCase names into something else.
    , camelTo
      -- * Other types
    , DotNetTime(..)
    )
where


-- ~
import Core
import Core.Control.Flow ((|>), (<|))
import Data.Monoid       ((<>), Monoid(..))
import Control.Arrow     ((>>>), (<<<))
import Prelude
    ( return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )

import qualified Prelude        as Pre
import qualified Core.Utils     as Core
import qualified Core.List.Util as Core

import qualified Control.Monad              as M
import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M
import qualified Control.Monad.Trans        as M

import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Foldable                as Fold
import qualified Data.Monoid                  as Monoid
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.Word                    as Word
import qualified Data.STRef                   as ST
import qualified Data.Bits                    as Bit
import qualified Data.Fixed                   as Fixed
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Data.Vector.Generic          as VG
import qualified Data.IORef                   as IORef
import qualified Data.ByteString              as BS

-- + C FFI
import qualified Foreign.C.Types as C

-- + OS APIS & Related
import qualified Path
import qualified System.Directory      as SD
import qualified System.FilePath.Posix as FP
import qualified System.Posix.Time     as Time

-- + Concurrency & Related
import qualified Control.Concurrent       as CC
import qualified Control.Concurrent.Async as Async

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP 


-- --------------
-- Web/GHCJS Specific
-- ---------------
import Data.Aeson.Types
  ( Parser, Result(..)
  , parse, parseEither, parseMaybe, modifyFailure
  , Options(..), SumEncoding(..), defaultOptions, defaultTaggedObject
  , camelTo
  , DotNetTime(..)
  )

import           Prelude           hiding (lookup)

import           Control.DeepSeq
import           Control.Exception

import           Data.Coerce
import           Data.Data
import qualified Data.JSString     as JSS
import           Data.JSString.Internal.Type (JSString(..))
import           Data.Maybe (fromMaybe)
import           Data.Typeable

import qualified GHC.Exts          as Exts
import           GHC.Types (IO(..))

import qualified GHCJS.Foreign     as F
import           GHCJS.Internal.Types
import           GHCJS.Types
import qualified GHCJS.Prim.Internal.Build as IB

import qualified JavaScript.Array          as A
import qualified JavaScript.Array.Internal as AI

import           Unsafe.Coerce


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~




data JSONException = UnknownKey
  deriving (Show, Typeable)

instance Exception JSONException

-- any JSON value
newtype SomeValue (m :: MutabilityType s) =
  SomeValue JSVal deriving (Typeable)
type Value        = SomeValue Immutable
type MutableValue = SomeValue Mutable
instance NFData (SomeValue (m :: MutabilityType s)) where
  rnf (SomeValue v) = rnf v

-- a dictionary (object)
newtype SomeObject (m :: MutabilityType s) =
  SomeObject JSVal deriving (Typeable)
type Object        = SomeObject Immutable
type MutableObject = SomeObject Mutable
instance NFData (SomeObject (m :: MutabilityType s)) where
  rnf (SomeObject v) = rnf v

{-
objectFromAssocs :: [(JSString, Value)] -> Object
objectFromAssocs xs = rnf xs `seq` js_objectFromAssocs (unsafeCoerce xs)
{-# INLINE objectFromAssocs #-}
-}

objectProperties :: Object -> AI.JSArray
objectProperties o = js_objectPropertiesPure o
{-# INLINE objectProperties #-}

objectPropertiesIO :: SomeObject o -> IO AI.JSArray
objectPropertiesIO o = js_objectProperties o
{-# INLINE objectPropertiesIO #-}

objectAssocs :: Object -> [(JSString, Value)]
objectAssocs o = unsafeCoerce (js_listAssocsPure o)
{-# INLINE objectAssocs #-}

objectAssocsIO :: SomeObject m -> IO [(JSString, Value)]
objectAssocsIO o = IO $ \s -> case js_listAssocs o s of
                                (# s', r #) -> (# s', unsafeCoerce r #)
{-# INLINE objectAssocsIO #-}

type Pair        = (JSString, Value)
type MutablePair = (JSString, MutableValue)

data SomeValue' (m :: MutabilityType s)
  = Object !(SomeObject m)
  | Array  !(AI.SomeJSArray m)
  | String !JSString
  | Number !Double
  | Bool   !Bool
  | Null
  deriving (Typeable)

type Value'        = SomeValue' Immutable
type MutableValue' = SomeValue' Mutable

-- -----------------------------------------------------------------------------
-- immutable lookup

class Lookup k a where
  (!)       :: k -> a -> Value             -- ^ throws when result is not a JSON value
  lookup    :: k -> a -> Maybe Value       -- ^ returns Nothing when result is not a JSON value
-- fixme more optimized matching
--  lookup'   :: k -> a -> Maybe Value'      -- ^ returns Nothing when result is not a JSON value

instance Lookup JSString Object where
  p ! d      = fromMaybe (throw UnknownKey) (lookup p d)
  lookup p d = let v = js_lookupDictPure p d
               in  if isUndefined v then Nothing else Just (SomeValue v)

instance Lookup JSString Value where
  p ! d      = fromMaybe (throw UnknownKey) (lookup p d)
  lookup p d = let v = js_lookupDictPureSafe p d
               in if isUndefined v then Nothing else Just (SomeValue v)

instance Lookup Int A.JSArray where
  i ! a      = fromMaybe (throw UnknownKey) (lookup i a)
  lookup i a = let v = js_lookupArrayPure i a
               in if isUndefined v then Nothing else Just (SomeValue v)
                                                     
instance Lookup Int Value where
  i ! a      = fromMaybe (throw UnknownKey) (lookup i a)
  lookup i a = let v = js_lookupArrayPureSafe i a
               in if isUndefined v then Nothing else Just (SomeValue v)

-- -----------------------------------------------------------------------------
-- mutable lookup

class IOLookup k a where
  (^!)      :: k -> a -> IO MutableValue          -- ^ throws when result is not a JSON value
  lookupIO  :: k -> a -> IO (Maybe MutableValue)  -- ^ returns Nothing when result is not a JSON value
  lookupIO' :: k -> a -> IO (Maybe MutableValue') -- ^ returns Nothing when result is not a JSON value

-- -----------------------------------------------------------------------------

match :: SomeValue m -> SomeValue' m
match (SomeValue v) =
  case F.jsonTypeOf v of
    F.JSONNull    -> Null
    F.JSONBool    -> Bool   (js_jsvalToBool v)
    F.JSONInteger -> Number (js_jsvalToDouble v)
    F.JSONFloat   -> Number (js_jsvalToDouble v)
    F.JSONString  -> String (JSString v)
    F.JSONArray   -> Array  (AI.SomeJSArray v)
    F.JSONObject  -> Object (SomeObject v)
{-# INLINE match #-}

emptyArray :: Value
emptyArray = js_emptyArray
{-# INLINE emptyArray #-}

isEmptyArray :: Value -> Bool
isEmptyArray v = js_isEmptyArray v
{-# INLINE isEmptyArray #-}

emptyObject :: Object
emptyObject = js_emptyObject
{-# INLINE emptyObject #-}

object :: [Pair] -> Object
object []      = js_emptyObject
object xs      = SomeObject (IB.buildObjectI $ coerce xs)
{-# INLINE object #-}

freeze :: MutableValue -> IO Value
freeze v = js_clone v
{-# INLINE freeze #-}

unsafeFreeze :: MutableValue -> IO Value
unsafeFreeze (SomeValue v) = pure (SomeValue v)
{-# INLINE unsafeFreeze #-}

thaw :: Value -> IO MutableValue
thaw v = js_clone v
{-# INLINE thaw #-}

unsafeThaw :: Value -> IO MutableValue
unsafeThaw (SomeValue v) = pure (SomeValue v)
{-# INLINE unsafeThaw #-}

-- -----------------------------------------------------------------------------
-- smart constructors

arrayValue :: AI.JSArray -> Value
arrayValue (AI.SomeJSArray a) = SomeValue a
{-# INLINE arrayValue #-}

stringValue :: JSString -> Value
stringValue (JSString x) = SomeValue x
{-# INLINE stringValue #-}

doubleValue :: Double -> Value
doubleValue d = SomeValue (js_doubleToJSVal d)
{-# INLINE doubleValue #-}

boolValue :: Bool -> Value
boolValue True  = js_trueValue
boolValue False = js_falseValue
{-# INLINE boolValue #-}

nullValue :: Value
nullValue = SomeValue F.jsNull

arrayValueList :: [Value] -> AI.JSArray
arrayValueList xs = A.fromList (coerce xs)
{-# INLINE arrayValueList #-}

indexV :: AI.JSArray -> Int -> Value
indexV a i = SomeValue (AI.index i a)
{-# INLINE indexV #-}

objectValue :: Object -> Value
objectValue (SomeObject o) = SomeValue o
{-# INLINE objectValue #-}

encode :: Value -> JSString
encode v = js_encode v
{-# INLINE encode #-}

-- -----------------------------------------------------------------------------

js_emptyArray :: Value
js_emptyArray = stub

js_emptyObject :: Object
js_emptyObject = stub

js_isEmptyArray :: Value -> Bool
js_isEmptyArray = stub

js_trueValue :: Value
js_trueValue = stub

js_falseValue :: Value
js_falseValue = stub


-- -----------------------------------------------------------------------------
-- types must be checked before using these conversions

js_jsvalToDouble :: JSVal -> Double
js_jsvalToDouble = stub
js_jsvalToBool   :: JSVal -> Bool
js_jsvalToBool = stub

-- -----------------------------------------------------------------------------
-- various lookups

js_lookupDictPure :: JSString -> Object -> JSVal
js_lookupDictPure = stub

js_lookupDictPureSafe :: JSString -> Value -> JSVal
js_lookupDictPureSafe = stub

js_lookupArrayPure :: Int -> A.JSArray -> JSVal
js_lookupArrayPure = stub

js_lookupArrayPureSafe :: Int -> Value -> JSVal
js_lookupArrayPureSafe = stub

js_doubleToJSVal :: Double -> JSVal
js_doubleToJSVal = stub

js_clone :: SomeValue m0 -> IO (SomeValue m1)
js_clone = stub


-- -----------------------------------------------------------------------------

js_objectPropertiesPure :: Object -> AI.JSArray
js_objectPropertiesPure = stub

js_objectProperties :: SomeObject m -> IO AI.JSArray
js_objectProperties = stub

js_listAssocsPure :: Object -> Exts.Any -- [(JSString, Value)]
js_listAssocsPure = stub

js_listAssocs :: SomeObject m -> Exts.State# s -> (# Exts.State# s, Exts.Any {- [(JSString, Value)] -} #)
js_listAssocs = stub

js_encode :: Value -> JSString
js_encode = stub





