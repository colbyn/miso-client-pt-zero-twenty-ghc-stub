{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash, DeriveDataTypeable, CPP #-}
module GHCJS.Prim
    ( JSVal(..)
    , WouldBlockException(..)
    , JSException(..)
    , mkJSException
    , fromJSString
    , toJSString
    , toJSArray
    , fromJSArray
    , fromJSInt
    , toJSInt
    , isNull
    , isUndefined
    , jsNull
    , getProp
    , getProp'
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

import           Data.Typeable (Typeable)
import           Unsafe.Coerce (unsafeCoerce)

import           GHC.Prim
import qualified GHC.Exception as Ex
import qualified GHC.Exts as Exts

-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


{-
  JSVal is a boxed type that can be used as FFI
  argument or result.
-}


-- REF:
-- #ifdef ghcjs_HOST_OS
-- data JSVal = JSVal ByteArray#
-- #else
-- data JSVal = JSVal Addr#
-- #endif

data JSVal = JSVal ByteArray#


{-
  When a JavaScript exception is raised inside
  a safe or interruptible foreign call, it is converted
  to a JSException
 -}
data JSException = JSException JSVal String
  deriving (Typeable)

instance Ex.Exception JSException

instance Show JSException where
  show (JSException _ xs) = "JavaScript exception: " ++ xs


mkJSException :: JSVal -> IO JSException
mkJSException = stub

{- | Low-level conversion utilities for packages that cannot
     depend on ghcjs-base
 -}

{- | returns an empty string if the JSVal does not contain
     a string
 -}
fromJSString :: JSVal -> String
fromJSString = stub
{-# INLINE fromJSString #-}

toJSString :: String -> JSVal
toJSString = stub
{-# INLINE toJSString #-}

fromJSArray :: JSVal -> IO [JSVal]
fromJSArray = stub
{-# INLINE fromJSArray #-}

toJSArray :: [JSVal] -> IO JSVal
toJSArray = stub
{-# INLINE toJSArray #-}

{- | returns zero if the JSVal does not contain a number
 -}
fromJSInt :: JSVal -> Int
fromJSInt = js_fromJSInt
{-# INLINE fromJSInt #-}

toJSInt :: Int -> JSVal
toJSInt = js_toJSInt
{-# INLINE toJSInt #-}

isNull :: JSVal -> Bool
isNull = js_isNull
{-# INLINE isNull #-}

isUndefined :: JSVal -> Bool
isUndefined = js_isUndefined
{-# INLINE isUndefined #-}

jsNull :: JSVal
jsNull = js_null
{-# INLINE jsNull #-}

getProp :: JSVal -> String -> IO JSVal
getProp = stub
{-# INLINE getProp #-}

getProp' :: JSVal -> JSVal -> IO JSVal
getProp' = stub
{-# INLINE getProp' #-}

-- reduce the spine and all list elements to whnf
seqList :: [a] -> [a]
seqList = stub

seqListSpine :: [a] -> [a]
seqListSpine = stub

js_fromJSString :: JSVal -> Exts.Any
js_fromJSString = stub
js_toJSString :: Exts.Any -> JSVal
js_toJSString = stub
js_fromJSArray :: JSVal -> IO Exts.Any
js_fromJSArray = stub
js_toJSArray :: Exts.Any -> IO JSVal
js_toJSArray = stub
js_isNull :: JSVal -> Bool
js_isNull = stub
js_isUndefined :: JSVal -> Bool
js_isUndefined = stub
js_fromJSInt :: JSVal -> Int
js_fromJSInt = stub
js_toJSInt :: Int -> JSVal
js_toJSInt = stub
js_null :: JSVal
js_null = stub
js_getProp :: JSVal -> Exts.Any -> IO JSVal
js_getProp = stub
js_getProp' :: JSVal -> JSVal -> IO JSVal
js_getProp' = stub


{- | If a synchronous thread tries to do something that can only
     be done asynchronously, and the thread is set up to not
     continue asynchronously, it receives this exception.
 -}
data WouldBlockException = WouldBlockException
  deriving (Typeable)

instance Show WouldBlockException where
  show _ = "thread would block"

instance Ex.Exception WouldBlockException
