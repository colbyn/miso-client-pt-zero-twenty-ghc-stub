{-# LANGUAGE NoImplicitPrelude #-}
module JavaScript.TypedArray.ArrayBuffer
    ( ArrayBuffer
    , MutableArrayBuffer
    , freeze, unsafeFreeze
    , thaw, unsafeThaw
    , byteLength
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
    , fmap
    , pure
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
import JavaScript.TypedArray.ArrayBuffer.Internal

import GHC.Exts
import GHC.Types


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


create :: Int -> IO MutableArrayBuffer
create n = fmap SomeArrayBuffer (IO (js_create n))
{-# INLINE create #-}

{- | Create an immutable 'ArrayBuffer' by copying a 'MutableArrayBuffer' -}
freeze :: MutableArrayBuffer -> IO ArrayBuffer
freeze (SomeArrayBuffer b) = fmap SomeArrayBuffer (IO (js_slice1 0 b))
{-# INLINE freeze #-}

{- | Create an immutable 'ArrayBuffer' from a 'MutableArrayBuffer' without
     copying. The result shares the buffer with the argument,  not modify
     the data in the 'MutableArrayBuffer' after freezing
 -}
unsafeFreeze :: MutableArrayBuffer -> IO ArrayBuffer
unsafeFreeze (SomeArrayBuffer b) = pure (SomeArrayBuffer b)
{-# INLINE unsafeFreeze #-}

{- | Create a 'MutableArrayBuffer' by copying an immutable 'ArrayBuffer' -}
thaw :: ArrayBuffer -> IO MutableArrayBuffer
thaw (SomeArrayBuffer b) = fmap SomeArrayBuffer (IO (js_slice1 0 b))
{-# INLINE thaw #-}

unsafeThaw :: ArrayBuffer -> IO MutableArrayBuffer
unsafeThaw (SomeArrayBuffer b) = pure (SomeArrayBuffer b)
{-# INLINE unsafeThaw #-}

slice :: Int -> Maybe Int -> SomeArrayBuffer any -> SomeArrayBuffer any
slice begin (Just end) b = js_slice_imm begin end b
slice begin _          b = js_slice1_imm begin b
{-# INLINE slice #-}

byteLength :: SomeArrayBuffer any -> Int
byteLength b = js_byteLength b
{-# INLINE byteLength #-}




