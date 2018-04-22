{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnliftedFFITypes #-}
module JavaScript.Object.Internal
    ( Object(..)
    , create
    , allProps
    , listProps
    , getProp
    , unsafeGetProp
    , setProp
    , unsafeSetProp
    , isInstanceOf
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
import           Data.JSString
import           Data.Typeable

import qualified GHCJS.Prim                as Prim
import           GHCJS.Types

import qualified JavaScript.Array          as JA
import           JavaScript.Array.Internal (JSArray, SomeJSArray(..))

import           Unsafe.Coerce
import qualified GHC.Exts as Exts


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


newtype Object = Object JSVal deriving (Typeable)
instance IsJSVal Object

-- | create an empty object
create :: IO Object
create = js_create
{-# INLINE create #-}

allProps :: Object -> IO JSArray
allProps o = js_allProps o
{-# INLINE allProps #-}

listProps :: Object -> IO [JSString]
listProps o = unsafeCoerce (js_listProps o)
{-# INLINE listProps #-}

{- | get a property from an object. If accessing the property results in
     an exception, the exception is converted to a JSException. Since exception
     handling code prevents some optimizations in some JS engines, you may want
     to use unsafeGetProp instead
 -}
getProp :: JSString -> Object -> IO JSVal
getProp p o = js_getProp p o
{-# INLINE getProp #-}

unsafeGetProp :: JSString -> Object -> IO JSVal
unsafeGetProp p o = js_unsafeGetProp p o
{-# INLINE unsafeGetProp #-}

setProp :: JSString -> JSVal -> Object -> IO ()
setProp p v o = js_setProp p v o
{-# INLINE setProp #-}

unsafeSetProp :: JSString -> JSVal -> Object -> IO ()
unsafeSetProp p v o = js_unsafeSetProp p v o
{-# INLINE unsafeSetProp #-}

isInstanceOf :: Object -> JSVal -> Bool
isInstanceOf o s = js_isInstanceOf o s
{-# INLINE isInstanceOf #-}

-- -----------------------------------------------------------------------------

js_create        :: IO Object
js_create = stub

js_getProp       :: JSString -> Object -> IO JSVal
js_getProp = stub

js_unsafeGetProp :: JSString -> Object -> IO JSVal
js_unsafeGetProp = stub

js_setProp       :: JSString -> JSVal -> Object -> IO ()
js_setProp = stub

js_unsafeSetProp :: JSString -> JSVal -> Object -> IO ()
js_unsafeSetProp = stub

js_isInstanceOf  :: Object -> JSVal -> Bool
js_isInstanceOf = stub

js_allProps      :: Object -> IO JSArray
js_allProps = stub

js_listProps     :: Object -> IO Exts.Any -- [JSString]
js_listProps = stub

