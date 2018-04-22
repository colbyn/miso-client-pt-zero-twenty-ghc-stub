{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE ForeignFunctionInterface, UnliftedFFITypes, JavaScriptFFI,
    UnboxedTuples, DeriveDataTypeable, GHCForeignImportPrim,
    MagicHash, FlexibleInstances, BangPatterns, Rank2Types, CPP #-}

{- | Conversion between 'Data.Text.Text' and 'Data.JSString.JSString'

 -}

module Data.JSString.Text
    ( textToJSString
    , textFromJSString
    , lazyTextToJSString
    , lazyTextFromJSString
    , textFromJSVal
    , lazyTextFromJSVal
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
import GHCJS.Prim

import GHC.Exts (ByteArray#, Int(..), Int#, Any)

import Control.DeepSeq

import qualified Data.Text.Array as A
import qualified Data.Text as T
import qualified Data.Text.Internal as T
import qualified Data.Text.Lazy as TL

import Data.JSString.Internal.Type

import Unsafe.Coerce


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


textToJSString :: T.Text -> JSString
textToJSString (T.Text (A.Array ba) (I# offset) (I# length)) =
  js_toString ba offset length
{-# INLINE textToJSString #-}

textFromJSString :: JSString -> T.Text
textFromJSString j =
  case js_fromString j of
    (# _ , 0#     #) -> T.empty
    (# ba, length #) -> T.Text (A.Array ba) 0 (I# length)
{-# INLINE  textFromJSString #-}

lazyTextToJSString :: TL.Text -> JSString
lazyTextToJSString t = rnf t `seq` js_lazyTextToString (unsafeCoerce t)
{-# INLINE lazyTextToJSString #-}

lazyTextFromJSString :: JSString -> TL.Text
lazyTextFromJSString = TL.fromStrict . textFromJSString
{-# INLINE lazyTextFromJSString #-}

-- | returns the empty Text if not a string
textFromJSVal :: JSVal -> T.Text
textFromJSVal j = case js_fromString' j of
    (# _,  0#     #) -> T.empty
    (# ba, length #) -> T.Text (A.Array ba) 0 (I# length)
{-# INLINE textFromJSVal #-}

-- | returns the empty Text if not a string
lazyTextFromJSVal :: JSVal -> TL.Text
lazyTextFromJSVal = TL.fromStrict . textFromJSVal
{-# INLINE lazyTextFromJSVal #-}

-- ----------------------------------------------------------------------------

js_toString :: ByteArray# -> Int# -> Int# -> JSString
js_toString = stub

js_fromString :: JSString -> (# ByteArray#, Int# #)
js_fromString = stub

js_fromString' :: JSVal -> (# ByteArray#, Int# #)
js_fromString' = stub

js_lazyTextToString :: Any -> JSString
js_lazyTextToString = stub




