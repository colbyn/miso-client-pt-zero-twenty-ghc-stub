{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module JavaScript.TypedArray.DataView.Internal where


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
import Data.Int
import Data.Typeable
import Data.Word

import GHC.Exts ( State# )

import GHCJS.Prim
import GHCJS.Internal.Types

import JavaScript.TypedArray.ArrayBuffer.Internal


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


newtype SomeDataView (a :: MutabilityType s) = SomeDataView JSVal
  deriving Typeable

type DataView        = SomeDataView Immutable
type MutableDataView = SomeDataView Mutable
type STDataView s    = SomeDataView (STMutable s)

#define JSU foreign import javascript unsafe
#define JSS foreign import javascript safe


js_dataView1 :: JSVal -> JSVal
js_dataView1 = stub
js_dataView2 :: Int -> JSVal -> SomeDataView m
js_dataView2 = stub
js_unsafeDataView2 :: Int -> JSVal-> SomeDataView m
js_unsafeDataView2 = stub
js_dataView :: Int -> Int -> JSVal -> SomeDataView m
js_dataView = stub
js_unsafeDataView :: Int -> Int -> JSVal -> JSVal
js_unsafeDataView = stub
js_cloneDataView :: SomeDataView m -> IO (SomeDataView m1)
js_cloneDataView = stub

-- ----------------------------------------------------------------------------
-- immutable getters

js_i_unsafeGetInt8       :: Int -> DataView -> Int8
js_i_unsafeGetInt8 = stub

js_i_unsafeGetUint8      :: Int -> DataView -> Word8
js_i_unsafeGetUint8 = stub

js_i_unsafeGetInt16BE    :: Int -> DataView -> Int16
js_i_unsafeGetInt16BE = stub

js_i_unsafeGetInt32BE    :: Int -> DataView -> Int
js_i_unsafeGetInt32BE = stub

js_i_unsafeGetUint16BE   :: Int -> DataView -> Word16
js_i_unsafeGetUint16BE = stub

js_i_unsafeGetUint32BE   :: Int -> DataView -> Word
js_i_unsafeGetUint32BE = stub

js_i_unsafeGetFloat32BE  :: Int -> DataView -> Double
js_i_unsafeGetFloat32BE = stub

js_i_unsafeGetFloat64BE  :: Int -> DataView -> Double
js_i_unsafeGetFloat64BE = stub

js_i_unsafeGetInt16LE    :: Int -> DataView -> Int16
js_i_unsafeGetInt16LE = stub

js_i_unsafeGetInt32LE    :: Int -> DataView -> Int
js_i_unsafeGetInt32LE = stub

js_i_unsafeGetUint16LE   :: Int -> DataView -> Word16
js_i_unsafeGetUint16LE = stub

js_i_unsafeGetUint32LE   :: Int -> DataView -> Word
js_i_unsafeGetUint32LE = stub

js_i_unsafeGetFloat32LE  :: Int -> DataView -> Double
js_i_unsafeGetFloat32LE = stub

js_i_unsafeGetFloat64LE  :: Int -> DataView -> Double
js_i_unsafeGetFloat64LE = stub

js_i_getInt8       :: Int -> DataView -> Int8
js_i_getInt8 = stub

js_i_getUint8      :: Int -> DataView -> Word8
js_i_getUint8 = stub

js_i_getInt16BE    :: Int -> DataView -> Int16
js_i_getInt16BE = stub

js_i_getInt32BE    :: Int -> DataView -> Int
js_i_getInt32BE = stub

js_i_getUint16BE   :: Int -> DataView -> Word16
js_i_getUint16BE = stub

js_i_getUint32BE   :: Int -> DataView -> Word
js_i_getUint32BE = stub

js_i_getFloat32BE  :: Int -> DataView -> Double
js_i_getFloat32BE = stub

js_i_getFloat64BE  :: Int -> DataView -> Double
js_i_getFloat64BE = stub

js_i_getInt16LE    :: Int -> DataView -> Int16
js_i_getInt16LE = stub

js_i_getInt32LE    :: Int -> DataView -> Int
js_i_getInt32LE = stub

js_i_getUint16LE   :: Int -> DataView -> Word16
js_i_getUint16LE = stub

js_i_getUint32LE   :: Int -> DataView -> Word
js_i_getUint32LE = stub

js_i_getFloat32LE  :: Int -> DataView -> Double
js_i_getFloat32LE = stub

js_i_getFloat64LE  :: Int -> DataView -> Double
js_i_getFloat64LE = stub


-- ----------------------------------------------------------------------------
-- mutable getters

js_m_unsafeGetInt8      :: Int -> SomeDataView m -> State# s -> (# State# s, Int8   #)
js_m_unsafeGetInt8 = stub
js_m_unsafeGetUint8     :: Int -> SomeDataView m -> State# s -> (# State# s, Word8  #)
js_m_unsafeGetUint8 = stub
js_m_unsafeGetInt16BE   :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
js_m_unsafeGetInt16BE = stub
js_m_unsafeGetInt32BE   :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
js_m_unsafeGetInt32BE = stub
js_m_unsafeGetUint16BE  :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
js_m_unsafeGetUint16BE = stub
js_m_unsafeGetUint32BE  :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
js_m_unsafeGetUint32BE = stub
js_m_unsafeGetFloat32BE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_unsafeGetFloat32BE = stub
js_m_unsafeGetFloat64BE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_unsafeGetFloat64BE = stub
js_m_unsafeGetInt16LE   :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
js_m_unsafeGetInt16LE = stub
js_m_unsafeGetInt32LE   :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
js_m_unsafeGetInt32LE = stub
js_m_unsafeGetUint16LE  :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
js_m_unsafeGetUint16LE = stub
js_m_unsafeGetUint32LE  :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
js_m_unsafeGetUint32LE = stub
js_m_unsafeGetFloat32LE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_unsafeGetFloat32LE = stub
js_m_unsafeGetFloat64LE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_unsafeGetFloat64LE = stub
js_m_getInt8            :: Int -> SomeDataView m -> State# s -> (# State# s, Int8   #)
js_m_getInt8 = stub
js_m_getUint8           :: Int -> SomeDataView m -> State# s -> (# State# s, Word8  #)
js_m_getUint8 = stub
js_m_getInt16BE         :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
js_m_getInt16BE = stub
js_m_getInt32BE         :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
js_m_getInt32BE = stub
js_m_getUint16BE        :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
js_m_getUint16BE = stub
js_m_getUint32BE        :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
js_m_getUint32BE = stub
js_m_getFloat32BE       :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_getFloat32BE = stub
js_m_getFloat64BE       :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_getFloat64BE = stub
js_m_getInt16LE         :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
js_m_getInt16LE = stub
js_m_getInt32LE         :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
js_m_getInt32LE = stub
js_m_getUint16LE        :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
js_m_getUint16LE = stub
js_m_getUint32LE        :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
js_m_getUint32LE = stub
js_m_getFloat32LE       :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_getFloat32LE = stub
js_m_getFloat64LE       :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_getFloat64LE = stub

-- ----------------------------------------------------------------------------
-- mutable setters

js_unsafeSetInt8      :: Int -> Int8   -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetInt8 = stub
js_unsafeSetUint8     :: Int -> Word8  -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetUint8 = stub
js_unsafeSetInt16BE   :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetInt16BE = stub
js_unsafeSetInt32BE   :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetInt32BE = stub
js_unsafeSetUint16BE  :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetUint16BE = stub
js_unsafeSetUint32BE  :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetUint32BE = stub
js_unsafeSetFloat32BE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetFloat32BE = stub
js_unsafeSetFloat64BE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetFloat64BE = stub
js_unsafeSetInt16LE   :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetInt16LE = stub
js_unsafeSetInt32LE   :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetInt32LE = stub
js_unsafeSetUint16LE  :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetUint16LE = stub
js_unsafeSetUint32LE  :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetUint32LE = stub
js_unsafeSetFloat32LE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetFloat32LE = stub
js_unsafeSetFloat64LE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetFloat64LE = stub
js_setInt8            :: Int -> Int8   -> SomeDataView m -> State# s -> (# State# s, () #)
js_setInt8 = stub
js_setUint8           :: Int -> Word8  -> SomeDataView m -> State# s -> (# State# s, () #)
js_setUint8 = stub
js_setInt16BE         :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
js_setInt16BE = stub
js_setInt32BE         :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
js_setInt32BE = stub
js_setUint16BE        :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
js_setUint16BE = stub
js_setUint32BE        :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
js_setUint32BE = stub
js_setFloat32BE       :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_setFloat32BE = stub
js_setFloat64BE       :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_setFloat64BE = stub
js_setInt16LE         :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
js_setInt16LE = stub
js_setInt32LE         :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
js_setInt32LE = stub
js_setUint16LE        :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
js_setUint16LE = stub
js_setUint32LE        :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
js_setUint32LE = stub
js_setFloat32LE       :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_setFloat32LE = stub
js_setFloat64LE       :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_setFloat64LE = stub







