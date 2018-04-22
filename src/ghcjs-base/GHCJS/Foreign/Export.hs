{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE EmptyDataDecls #-}

{- | 
     Dynamically export Haskell values to JavaScript
-}
module GHCJS.Foreign.Export (
    Export
  , export
  , withExport
  , derefExport
  , releaseExport
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
    , undefined
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
import Control.Exception (bracket)
import GHC.Exts (Any)
import GHC.Fingerprint
import Data.Typeable
import Data.Word
import Unsafe.Coerce
import qualified GHC.Exts as Exts

import GHCJS.Prim
import GHCJS.Types


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


newtype Export a = Export JSVal
instance IsJSVal (Export a)

{- |
     Export any Haskell value to a JavaScript reference without evaluating it.
     The JavaScript reference can be passed to foreign code and used to retrieve
     the value later.

     The data referenced by the value will be kept in memory until you call
     'releaseExport', even if no foreign code references the export anymore.
 -}
export :: Typeable a => a -> IO (Export a)
export x = js_export w1 w2 (unsafeCoerce x)
  where
    Fingerprint w1 w2 = typeRepFingerprint (typeOf x)

{- |
     Export the value and run the action. The value is only exported for the
     duration of the action. Dereferencing it after the 'withExport' call
     has returned will always return 'Nothing'.
 -}
-- fixme is this safe with nested exports?
withExport :: Typeable a => a -> (Export a -> IO b) -> IO b
withExport x m = bracket (export x) releaseExport m

{- |
     Retrieve the Haskell value from an export. Returns 'Nothing' if the
     type does not match or the export has already been released.
 -}

derefExport :: forall a. Typeable a => Export a -> IO (Maybe a)
derefExport e = do
  let Fingerprint w1 w2 = typeRepFingerprint (typeOf (undefined :: a))
  r <- js_derefExport w1 w2 e
  if isNull r
    then return Nothing
    else Just . unsafeCoerce <$> js_toHeapObject r

{- |
     Release all memory associated with the export. Subsequent calls to
     'derefExport' will return 'Nothing'
 -}
releaseExport :: Export a -> IO ()
releaseExport e = js_releaseExport e

-- ----------------------------------------------------------------------------


js_export :: Word64 -> Word64 -> Any -> IO (Export a)
js_export = stub

js_derefExport :: Word64 -> Word64 -> Export a -> IO JSVal
js_derefExport = stub

js_toHeapObject :: JSVal -> IO Any
js_toHeapObject = stub

js_releaseExport :: Export a -> IO ()
js_releaseExport = stub




