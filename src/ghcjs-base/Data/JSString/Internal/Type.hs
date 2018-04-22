{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP, DeriveDataTypeable, UnboxedTuples, MagicHash,
             BangPatterns, ForeignFunctionInterface, JavaScriptFFI #-}
{-# OPTIONS_HADDOCK not-home #-}
module Data.JSString.Internal.Type (
    JSString(..)
  , empty
  , empty_
  , safe
  , firstf
) where


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
import Control.DeepSeq

import Data.Bits
import Data.Int                       (Int32, Int64)
-- import Data.Text.Internal.Unsafe.Char (ord)
import Data.Typeable                  (Typeable)
import GHC.Exts                       (Char(..), ord#, andI#, (/=#), isTrue#)

import GHCJS.Prim (JSVal)

import GHCJS.Internal.Types


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


-- | A wrapper around a JavaScript string
newtype JSString = JSString JSVal
instance IsJSVal JSString

instance NFData JSString where rnf !x = ()


js_empty :: JSString
js_empty = stub

-- | /O(1)/ The empty 'JSString'.
empty :: JSString
empty = js_empty
{-# INLINE [1] empty #-}

-- | A non-inlined version of 'empty'.
empty_ :: JSString
empty_ = js_empty
{-# NOINLINE empty_ #-}

safe :: Char -> Char
safe c@(C# cc)
    | isTrue# (andI# (ord# cc) 0x1ff800# /=# 0xd800#) = c
    | otherwise                    = '\xfffd'
{-# INLINE [0] safe #-}


-- | Apply a function to the first element of an optional pair.
firstf :: (a -> c) -> Maybe (a,b) -> Maybe (c,b)
firstf f (Just (a, b)) = Just (f a, b)
firstf _  Nothing      = Nothing

{-
-- | Checked multiplication.  Calls 'error' if the result would
-- overflow.
mul :: Int -> Int -> Int
#if WORD_SIZE_IN_BITS == 64
mul a b = fromIntegral $ fromIntegral a `mul64` fromIntegral b
#else
mul a b = fromIntegral $ fromIntegral a `mul32` fromIntegral b
#endif
{-# INLINE mul #-}
infixl 7 `mul`

-- | Checked multiplication.  Calls 'error' if the result would
-- overflow.
mul64 :: Int64 -> Int64 -> Int64
mul64 a b
  | a >= 0 && b >= 0 =  mul64_ a b
  | a >= 0           = -mul64_ a (-b)
  | b >= 0           = -mul64_ (-a) b
  | otherwise        =  mul64_ (-a) (-b)
{-# INLINE mul64 #-}
infixl 7 `mul64`

mul64_ :: Int64 -> Int64 -> Int64
mul64_ a b
  | ahi > 0 && bhi > 0 = error "overflow"
  | top > 0x7fffffff   = error "overflow"
  | total < 0          = error "overflow"
  | otherwise          = total
  where (# ahi, alo #) = (# a `shiftR` 32, a .&. 0xffffffff #)
        (# bhi, blo #) = (# b `shiftR` 32, b .&. 0xffffffff #)
        top            = ahi * blo + alo * bhi
        total          = (top `shiftL` 32) + alo * blo
{-# INLINE mul64_ #-}

-- | Checked multiplication.  Calls 'error' if the result would
-- overflow.
mul32 :: Int32 -> Int32 -> Int32
mul32 a b = case fromIntegral a * fromIntegral b of
              ab | ab < min32 || ab > max32 -> error "overflow"
                 | otherwise                -> fromIntegral ab
  where min32 = -0x80000000 :: Int64
        max32 =  0x7fffffff
{-# INLINE mul32 #-}
infixl 7 `mul32`
-}




