{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, UnliftedFFITypes,
             GHCForeignImportPrim, UnboxedTuples, BangPatterns,
             MagicHash
#-}
module Data.JSString.Read
    ( isInteger
    , isNatural
    , readInt
    , readIntMaybe
    , lenientReadInt
    , readInt64
    , readInt64Maybe
    , readWord64
    , readWord64Maybe
    , readDouble
    , readDoubleMaybe
    , readInteger
    , readIntegerMaybe
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
import GHCJS.Types

import GHC.Exts (Any, Int#, Int64#, Word64#, Int(..))
import GHC.Int (Int64(..))
import GHC.Word (Word64(..))
import Unsafe.Coerce
import Data.Maybe
import Data.JSString


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~




{- |
    Returns whether the JSString represents an integer at base 10
 -}
isInteger :: JSString -> Bool
isInteger j = js_isInteger j
{-# INLINE isInteger #-}

{- |
    Returns whether the JSString represents a natural number at base 10
    (including 0)
 -}
isNatural :: JSString -> Bool
isNatural j = js_isInteger j
{-# INLINE isNatural #-}

{- |
     Convert a JSString to an Int, throwing an exception if it cannot
     be converted. Leading spaces are allowed. The function ignores
     trailing non-digit characters.
 -}
lenientReadInt :: JSString -> Int
lenientReadInt j = fromMaybe (readError "lenientReadInt") (lenientReadIntMaybe j)
{-# INLINE lenientReadInt #-}

{- |
     Convert a JSString to an Int, returning Nothing if it cannot
     be converted. Leading spaces are allowed. The function ignores
     trailing non-digit characters.
 -}
lenientReadIntMaybe :: JSString -> Maybe Int
lenientReadIntMaybe j = convertNullMaybe js_lenientReadInt j
{-# INLINE lenientReadIntMaybe #-}

{- |
     Convert a JSString to an Int, throwing an exception if it cannot
     be converted. Leading spaces and trailing non-digit characters
     are not allowed.
 -}
readInt :: JSString -> Int
readInt j = fromMaybe (readError "readInt") (readIntMaybe j)
{-# INLINE readInt #-}

{- |
     Convert a JSString to an Int, returning Nothing if it cannot
     be converted. Leading spaces and trailing non-digit characters
     are not allowed.
 -}
readIntMaybe :: JSString -> Maybe Int
readIntMaybe j = convertNullMaybe js_readInt j
{-# INLINE readIntMaybe #-}

readInt64 :: JSString -> Int64
readInt64 j = fromMaybe (readError "readInt64") (readInt64Maybe j)
{-# INLINE readInt64 #-}

readInt64Maybe :: JSString -> Maybe Int64
readInt64Maybe j = stub
{-# INLINE readInt64Maybe #-}

readWord64 :: JSString -> Word64
readWord64 j = fromMaybe (readError "readWord64") (readWord64Maybe j)
{-# INLINE readWord64 #-}

readWord64Maybe :: JSString -> Maybe Word64
readWord64Maybe j = stub
{-# INLINE readWord64Maybe #-}


{- |
     Convert a JSString to an Int, throwing an exception if it cannot
     be converted. Leading spaces are allowed. The function ignores
     trailing non-digit characters.
 -}
readDouble :: JSString -> Double
readDouble j = fromMaybe (readError "readDouble") (readDoubleMaybe j)
{-# INLINE readDouble #-}

{- |
     Convert a JSString to a Double, returning Nothing if it cannot
     be converted. Leading spaces are allowed. The function ignores
     trailing non-digit characters.
 -}

readDoubleMaybe :: JSString -> Maybe Double
readDoubleMaybe j = convertNullMaybe js_readDouble j
{-# INLINE readDoubleMaybe #-}

{- |
     Convert a JSString to a Double, returning Nothing if it cannot
     be converted. Leading spaces and trailing non-digit characters
     are not allowed.
 -}
strictReadDoubleMaybe :: JSString -> Maybe Double
strictReadDoubleMaybe j = convertNullMaybe js_readDouble j
{-# INLINE strictReadDoubleMaybe #-}

readInteger :: JSString -> Integer
readInteger j = fromMaybe (readError "readInteger") (readIntegerMaybe j)
{-# INLINE readInteger #-}

readIntegerMaybe :: JSString -> Maybe Integer
readIntegerMaybe j = convertNullMaybe js_readInteger j
{-# INLINE readIntegerMaybe #-}

-- ----------------------------------------------------------------------------

convertNullMaybe :: (JSString -> JSVal) -> JSString -> Maybe a
convertNullMaybe f j
  | js_isNull r = Nothing
  | otherwise   = Just (unsafeCoerce (js_toHeapObject r))
  where
    r = f j
{-# INLINE convertNullMaybe #-}

readError :: String -> a
readError xs = error ("Data.JSString.Read." ++ xs)

-- ----------------------------------------------------------------------------

js_isNull :: JSVal -> Bool
js_isNull = stub

js_toHeapObject :: JSVal -> Any
js_toHeapObject = stub

js_readInteger :: JSString -> JSVal
js_readInteger = stub

js_readInt :: JSString -> JSVal
js_readInt = stub

js_lenientReadInt :: JSString -> JSVal
js_lenientReadInt = stub

js_readInt64 :: JSString -> (# Int#, Int64# #)
js_readInt64 = stub

js_readWord64 :: JSString -> (# Int#, Word64# #)
js_readWord64 = stub

js_readDouble :: JSString -> JSVal
js_readDouble = stub

js_isInteger :: JSString -> Bool
js_isInteger = stub

js_isNatural :: JSString -> Bool
js_isNatural = stub

