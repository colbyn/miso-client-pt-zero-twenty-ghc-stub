{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, DataKinds, KindSignatures,
             PolyKinds, UnboxedTuples, GHCForeignImportPrim, DeriveDataTypeable,
             UnliftedFFITypes, MagicHash
  #-}
module JavaScript.Array.Internal where


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
import           Prelude hiding (length, reverse, drop, take)

import           Control.DeepSeq
import           Data.Typeable
import           Unsafe.Coerce (unsafeCoerce)

import           GHC.Types
import           GHC.IO
import qualified GHC.Exts as Exts
import           GHC.Exts (State#)

import           GHCJS.Internal.Types
import qualified GHCJS.Prim as Prim
import           GHCJS.Types



-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~



newtype SomeJSArray (m :: MutabilityType s) = SomeJSArray JSVal
  deriving (Typeable)
instance IsJSVal (SomeJSArray m)

type JSArray        = SomeJSArray Immutable
type MutableJSArray = SomeJSArray Mutable

type STJSArray s    = SomeJSArray (STMutable s)

create :: IO MutableJSArray
create = IO js_create
{-# INLINE create #-}

length :: JSArray -> Int
length x = js_lengthPure x
{-# INLINE length #-}

lengthIO :: SomeJSArray m -> IO Int
lengthIO x = IO (js_length x)
{-# INLINE lengthIO #-}

null :: JSArray -> Bool
null x = length x == 0
{-# INLINE null #-}

append :: SomeJSArray m -> SomeJSArray m -> IO (SomeJSArray m1)
append x y = IO (js_append x y)
{-# INLINE append #-}

fromList :: [JSVal] -> JSArray
fromList xs = rnf xs `seq` js_toJSArrayPure (unsafeCoerce xs)
{-# INLINE fromList #-}

fromListIO :: [JSVal] -> IO (SomeJSArray m)
fromListIO xs = IO (\s -> rnf xs `seq` js_toJSArray (unsafeCoerce xs) s)
{-# INLINE fromListIO #-}

toList :: JSArray -> [JSVal]
toList x = unsafeCoerce (js_fromJSArrayPure x)
{-# INLINE toList #-}

toListIO :: SomeJSArray m -> IO [JSVal]
toListIO x = IO $ \s -> case js_fromJSArray x s of
                          (# s', xs #) -> (# s', unsafeCoerce xs #)
{-# INLINE toListIO #-}

index :: Int -> JSArray -> JSVal
index n x = js_indexPure n x
{-# INLINE index #-}

read :: Int -> SomeJSArray m -> IO JSVal
read n x = IO (js_index n x)
{-# INLINE read #-}

write :: Int -> JSVal -> MutableJSArray -> IO ()
write n e x = IO (js_setIndex n e x)
{-# INLINE write #-}

push :: JSVal -> MutableJSArray -> IO ()
push e x = IO (js_push e x)
{-# INLINE push #-}

pop :: MutableJSArray -> IO JSVal
pop x = IO (js_pop x)
{-# INLINE pop #-}

unshift :: JSVal -> MutableJSArray -> IO ()
unshift e x = IO (js_unshift e x)
{-# INLINE unshift #-}

shift :: MutableJSArray -> IO JSVal
shift x = IO (js_shift x)
{-# INLINE shift #-}

reverse :: MutableJSArray -> IO ()
reverse x = IO (js_reverse x)
{-# INLINE reverse #-}

take :: Int -> JSArray -> JSArray
take n x = js_slicePure 0 n x
{-# INLINE take #-}

takeIO :: Int -> SomeJSArray m -> IO (SomeJSArray m1)
takeIO n x = IO (js_slice 0 n x)
{-# INLINE takeIO #-}

drop :: Int -> JSArray -> JSArray
drop n x = js_slice1Pure n x
{-# INLINE drop #-}

dropIO :: Int -> SomeJSArray m -> IO (SomeJSArray m1)
dropIO n x = IO (js_slice1 n x)
{-# INLINE dropIO #-}

sliceIO :: Int -> Int -> JSArray -> IO (SomeJSArray m1)
sliceIO s n x = IO (js_slice s n x)
{-# INLINE sliceIO #-}

slice :: Int -> Int -> JSArray -> JSArray
slice s n x = js_slicePure s n x
{-# INLINE slice #-}

freeze :: MutableJSArray -> IO JSArray
freeze x = IO (js_slice1 0 x)
{-# INLINE freeze #-}

unsafeFreeze :: MutableJSArray -> IO JSArray
unsafeFreeze (SomeJSArray x) = pure (SomeJSArray x)
{-# INLINE unsafeFreeze #-}

thaw :: JSArray -> IO MutableJSArray
thaw x = IO (js_slice1 0 x)
{-# INLINE thaw #-}

unsafeThaw :: JSArray -> IO MutableJSArray
unsafeThaw (SomeJSArray x) = pure (SomeJSArray x)
{-# INLINE unsafeThaw #-}


-- -----------------------------------------------------------------------------

js_create   :: State# s -> (# State# s, SomeJSArray m #)
js_create = stub

js_length     :: SomeJSArray m -> State# s -> (# State# s, Int #)
js_length = stub

js_index     :: Int -> SomeJSArray m -> State# s -> (# State# s, JSVal #)
js_index = stub

js_indexPure :: Int -> JSArray -> JSVal
js_indexPure = stub

js_lengthPure :: JSArray -> Int
js_lengthPure = stub

js_setIndex :: Int -> JSVal -> SomeJSArray m -> State# s -> (# State# s, () #)
js_setIndex = stub

js_slice     :: Int -> Int -> SomeJSArray m -> State# s -> (# State# s, SomeJSArray m1 #)
js_slice = stub

js_slice1    :: Int -> SomeJSArray m -> State# s -> (# State# s, SomeJSArray m1 #)
js_slice1 = stub

js_slicePure  :: Int -> Int -> JSArray -> JSArray
js_slicePure = stub

js_slice1Pure :: Int -> JSArray -> JSArray
js_slice1Pure = stub

js_append   :: SomeJSArray m0 -> SomeJSArray m1 -> State# s ->  (# State# s, SomeJSArray m2 #)
js_append = stub

js_push     :: JSVal -> SomeJSArray m -> State# s -> (# State# s, () #)
js_push = stub

js_pop      :: SomeJSArray m -> State# s -> (# State# s, JSVal #)
js_pop = stub

js_unshift  :: JSVal -> SomeJSArray m -> State# s -> (# State# s, () #)
js_unshift = stub

js_shift    :: SomeJSArray m -> State# s -> (# State# s, JSVal #)
js_shift = stub

js_reverse  :: SomeJSArray m -> State# s -> (# State# s, () #)
js_reverse = stub

js_fromJSArray :: SomeJSArray m -> State# s -> (# State# s, Exts.Any #)
js_fromJSArray = stub

js_fromJSArrayPure :: JSArray -> Exts.Any -- [JSVal]
js_fromJSArrayPure = stub

js_toJSArray :: Exts.Any -> State# s -> (# State# s, SomeJSArray m #)
js_toJSArray = stub

js_toJSArrayPure :: Exts.Any -> JSArray
js_toJSArrayPure = stub





