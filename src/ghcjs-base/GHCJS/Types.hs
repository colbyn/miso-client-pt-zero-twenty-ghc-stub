{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
module GHCJS.Types
    ( JSVal
    , WouldBlockException(..)
    , JSException(..)
    , IsJSVal
    , jsval
    , isNull
    , isUndefined
    , nullRef
    , JSString
    , mkRef
    , Ref#
    , toPtr
    , fromPtr
    , JSRef
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

import Data.JSString.Internal.Type (JSString)
import GHCJS.Internal.Types

import GHCJS.Prim

import GHC.Int
import GHC.Types
import GHC.Prim
import GHC.Ptr

import Control.DeepSeq
import Unsafe.Coerce



-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~



type Ref# = ByteArray#

mkRef :: ByteArray# -> JSVal
mkRef x = JSVal x

nullRef :: JSVal
nullRef = js_nullRef
{-# INLINE nullRef #-}

toPtr :: JSVal -> Ptr a
toPtr (JSVal x) = unsafeCoerce (Ptr' x 0#)
{-# INLINE toPtr #-}

fromPtr :: Ptr a -> JSVal
fromPtr p = js_ptrVal p
{-# INLINE fromPtr #-}

data Ptr' a = Ptr' ByteArray# Int#

js_nullRef :: JSVal
js_nullRef = stub

js_ptrVal  :: Ptr a -> JSVal
js_ptrVal = stub

js_mkPtr :: JSVal -> Ptr a
js_mkPtr = stub

-- | This is a deprecated copmatibility wrapper for the old JSRef type.
--
type JSRef a = JSVal






