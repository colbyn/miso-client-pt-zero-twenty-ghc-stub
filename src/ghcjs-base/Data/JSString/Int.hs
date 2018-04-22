{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GHCForeignImportPrim #-}

module Data.JSString.Int
    ( decimal
    , hexadecimal
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
    , Integral(..)
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
import Data.JSString

import Data.Monoid

import GHC.Int
import GHC.Word
import GHC.Exts ( ByteArray#
                , Int(..), Int#, Int64#
                , Word(..), Word#, Word64#
                , (<#), (<=#), isTrue# )

import GHC.Integer.GMP.Internals
import Unsafe.Coerce
import GHCJS.Prim


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


decimal :: Integral a => a -> JSString
decimal i = decimal' i
{-# RULES "decimal/Int"     decimal = decimalI       :: Int     -> JSString #-}
{-# RULES "decimal/Int8"    decimal = decimalI8      :: Int8    -> JSString #-}
{-# RULES "decimal/Int16"   decimal = decimalI16     :: Int16   -> JSString #-}
{-# RULES "decimal/Int32"   decimal = decimalI32     :: Int32   -> JSString #-}
{-# RULES "decimal/Int64"   decimal = decimalI64     :: Int64   -> JSString #-}
{-# RULES "decimal/Word"    decimal = decimalW       :: Word    -> JSString #-}
{-# RULES "decimal/Word8"   decimal = decimalW8      :: Word8   -> JSString #-}
{-# RULES "decimal/Word16"  decimal = decimalW16     :: Word16  -> JSString #-}
{-# RULES "decimal/Word32"  decimal = decimalW32     :: Word32  -> JSString #-}
{-# RULES "decimal/Word64"  decimal = decimalW64     :: Word64  -> JSString #-}
{-# RULES "decimal/Integer" decimal = decimalInteger :: Integer -> JSString #-}
{-# SPECIALIZE decimal :: Integer -> JSString #-}
{-# SPECIALIZE decimal :: Int    -> JSString #-}
{-# SPECIALIZE decimal :: Int8   -> JSString #-}
{-# SPECIALIZE decimal :: Int16  -> JSString #-}
{-# SPECIALIZE decimal :: Int32  -> JSString #-}
{-# SPECIALIZE decimal :: Int64  -> JSString #-}
{-# SPECIALIZE decimal :: Word   -> JSString #-}
{-# SPECIALIZE decimal :: Word8  -> JSString #-}
{-# SPECIALIZE decimal :: Word16 -> JSString #-}
{-# SPECIALIZE decimal :: Word32 -> JSString #-}
{-# SPECIALIZE decimal :: Word64 -> JSString #-}
{-# INLINE [1] decimal #-}

decimalI :: Int -> JSString
decimalI (I# x) = js_decI x
{-# INLINE decimalI #-}

decimalI8 :: Int8 -> JSString
decimalI8 (I8# x) = js_decI x
{-# INLINE decimalI8 #-}

decimalI16 :: Int16 -> JSString
decimalI16 (I16# x) = js_decI x
{-# INLINE decimalI16 #-}

decimalI32 :: Int32 -> JSString
decimalI32 (I32# x) = js_decI x
{-# INLINE decimalI32 #-}

decimalI64 :: Int64 -> JSString
decimalI64 = stub
{-# INLINE decimalI64 #-}

decimalW8 :: Word8 -> JSString
decimalW8 (W8# x) = js_decW x
{-# INLINE decimalW8 #-}

decimalW16 :: Word16 -> JSString
decimalW16 (W16# x) = js_decW x
{-# INLINE decimalW16 #-}

decimalW32 :: Word32 -> JSString
decimalW32 (W32# x) = js_decW32 x
{-# INLINE decimalW32 #-}

decimalW64 :: Word64 -> JSString
decimalW64 = stub
{-# INLINE decimalW64 #-}

decimalW :: Word -> JSString
decimalW (W# x) = js_decW32 x
{-# INLINE decimalW #-}

-- hack warning, we should really expose J# somehow
data MyI = MyS Int# | MyJ Int# ByteArray#

decimalInteger :: Integer -> JSString
decimalInteger !i = js_decInteger (unsafeCoerce i)
{-# INLINE decimalInteger #-}

decimal' :: Integral a => a -> JSString
decimal' i = decimalInteger (toInteger i)
{-# NOINLINE decimal' #-}
{-
  | i < 0 = if i <= -10
              then let (q, r)   = i `quotRem` (-10)
                       !(I# rr) = fromIntegral r
                   in  js_minusDigit (positive q) rr
              else js_minus (positive (negate i))
  | otherwise = positive i

positive :: (Integral a) => a -> JSString
positive i
  | toInteger i < 1000000000 = let !(I# x) = fromIntegral i in js_decI x
  | otherwise                = let (q, r)  = i `quotRem` 1000000000
                                   !(I# x) = fromIntegral r
                               in  positive q <> js_decIPadded9 x
-}

hexadecimal :: Integral a => a -> JSString
hexadecimal i = hexadecimal' i
{-# RULES "hexadecimal/Int"     hexadecimal = hexI       :: Int     -> JSString #-}
{-# RULES "hexadecimal/Int8"    hexadecimal = hexI8      :: Int8    -> JSString #-}
{-# RULES "hexadecimal/Int16"   hexadecimal = hexI16     :: Int16   -> JSString #-}
{-# RULES "hexadecimal/Int32"   hexadecimal = hexI32     :: Int32   -> JSString #-}
{-# RULES "hexadecimal/Int64"   hexadecimal = hexI64     :: Int64   -> JSString #-}
{-# RULES "hexadecimal/Word"    hexadecimal = hexW       :: Word    -> JSString #-}
{-# RULES "hexadecimal/Word8"   hexadecimal = hexW8      :: Word8   -> JSString #-}
{-# RULES "hexadecimal/Word16"  hexadecimal = hexW16     :: Word16  -> JSString #-}
{-# RULES "hexadecimal/Word32"  hexadecimal = hexW32     :: Word32  -> JSString #-}
{-# RULES "hexadecimal/Word64"  hexadecimal = hexW64     :: Word64  -> JSString #-}
{-# RULES "hexadecimal/Integer" hexadecimal = hexInteger :: Integer -> JSString #-}
{-# SPECIALIZE hexadecimal :: Integer -> JSString #-}
{-# SPECIALIZE hexadecimal :: Int    -> JSString #-}
{-# SPECIALIZE hexadecimal :: Int8   -> JSString #-}
{-# SPECIALIZE hexadecimal :: Int16  -> JSString #-}
{-# SPECIALIZE hexadecimal :: Int32  -> JSString #-}
{-# SPECIALIZE hexadecimal :: Int64  -> JSString #-}
{-# SPECIALIZE hexadecimal :: Word   -> JSString #-}
{-# SPECIALIZE hexadecimal :: Word8  -> JSString #-}
{-# SPECIALIZE hexadecimal :: Word16 -> JSString #-}
{-# SPECIALIZE hexadecimal :: Word32 -> JSString #-}
{-# SPECIALIZE hexadecimal :: Word64 -> JSString #-}
{-# INLINE [1] hexadecimal #-}

hexadecimal' :: Integral a => a -> JSString
hexadecimal' i
    | i < 0     = error hexErrMsg
    | otherwise = hexInteger (toInteger i)
{-# NOINLINE hexadecimal' #-}

hexInteger :: Integer -> JSString
hexInteger !i
  | i < 0     = error hexErrMsg
  | otherwise = js_hexInteger (unsafeCoerce i)
{-# INLINE hexInteger #-}

hexI :: Int -> JSString
hexI (I# x) = if isTrue# (x <# 0#)
              then error hexErrMsg
              else js_hexI x
{-# INLINE hexI #-}

hexI8 :: Int8 -> JSString
hexI8 (I8# x) =
  if isTrue# (x <# 0#)
  then error hexErrMsg
  else js_hexI x
{-# INLINE hexI8 #-}

hexI16 :: Int16 -> JSString
hexI16 (I16# x) =
  if isTrue# (x <# 0#)
  then error hexErrMsg
  else js_hexI x
{-# INLINE hexI16 #-}

hexI32 :: Int32 -> JSString
hexI32 (I32# x) =
  if isTrue# (x <# 0#)
  then error hexErrMsg
  else js_hexI x
{-# INLINE hexI32 #-}

hexI64 :: Int64 -> JSString
hexI64 = stub
{-# INLINE hexI64 #-}

hexW :: Word -> JSString
hexW = stub
{-# INLINE hexW #-}

hexW8 :: Word8 -> JSString
hexW8 (W8# x) = js_hexW x
{-# INLINE hexW8 #-}

hexW16 :: Word16 -> JSString
hexW16 (W16# x) = js_hexW x
{-# INLINE hexW16 #-}

hexW32 :: Word32 -> JSString
hexW32 (W32# x) = js_hexW32 x
{-# INLINE hexW32 #-}

hexW64 :: Word64 -> JSString
hexW64 = stub
{-# INLINE hexW64 #-}

hexErrMsg :: String
hexErrMsg = "Data.JSString.Int.hexadecimal: applied to negative number"

-- ----------------------------------------------------------------------------

js_decI       :: Int#     -> JSString
js_decI = stub
js_decI64     :: Int64#   -> JSString
js_decI64 = stub
js_decW       :: Word#    -> JSString
js_decW = stub
js_decW32     :: Word#    -> JSString
js_decW32 = stub
js_decW64     :: Word64#  -> JSString
js_decW64 = stub
js_decInteger :: Any -> JSString
js_decInteger = stub

-- these are expected to be only applied to nonnegative integers
js_hexI       :: Int#    -> JSString
js_hexI = stub

js_hexI64     :: Int64#   -> JSString
js_hexI64 = stub

js_hexW       :: Word#    -> JSString
js_hexW = stub

js_hexW32     :: Word#    -> JSString
js_hexW32 = stub

js_hexW64     :: Word64#  -> JSString
js_hexW64 = stub

js_hexInteger :: Any -> JSString
js_hexInteger = stub


js_minusDigit :: JSString -> Int# -> JSString
js_minusDigit = stub

js_minus :: JSString -> JSString
js_minus = stub


--
js_decIPadded9 :: Int# -> JSString
js_decIPadded9 = stub

js_hexIPadded8 :: Int# -> JSString
js_hexIPadded8 = stub




