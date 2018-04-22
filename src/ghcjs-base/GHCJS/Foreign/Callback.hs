{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, UnliftedFFITypes,
             GHCForeignImportPrim, DeriveDataTypeable, GHCForeignImportPrim #-}
module GHCJS.Foreign.Callback
    ( Callback
    , OnBlocked(..)
    , releaseCallback
    -- * asynchronous callbacks
    , asyncCallback
    , asyncCallback1
    , asyncCallback2
    , asyncCallback3
    -- * synchronous callbacks
    , syncCallback
    , syncCallback1
    , syncCallback2
    , syncCallback3
    -- * synchronous callbacks that return a value
    , syncCallback'
    , syncCallback1'
    , syncCallback2'
    , syncCallback3'
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
import           GHCJS.Concurrent
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import           GHCJS.Foreign.Callback.Internal
import           GHCJS.Prim
import           GHCJS.Types

import qualified GHC.Exts as Exts

import           Data.Typeable

import           Unsafe.Coerce


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~



{- |
     When you create a callback, the Haskell runtime stores a reference to
     the exported IO action or function. This means that all data referenced by the
     exported value stays in memory, even if nothing outside the Haskell runtime
     holds a reference to to callback.

     Use 'releaseCallback' to free the reference. Subsequent calls from JavaScript
     to the callback will result in an exception.
 -}
releaseCallback :: Callback a -> IO ()
releaseCallback x = js_release x

{- | Make a callback (JavaScript function) that runs the supplied IO action in a synchronous
     thread when called.

     Call 'releaseCallback' when done with the callback, freeing memory referenced
     by the IO action.
 -}
syncCallback :: OnBlocked                               -- ^ what to do when the thread blocks
             -> IO ()                                   -- ^ the Haskell action
             -> IO (Callback (IO ()))                   -- ^ the callback
syncCallback onBlocked x = js_syncCallback (onBlocked == ContinueAsync) (unsafeCoerce x)


{- | Make a callback (JavaScript function) that runs the supplied IO function in a synchronous
     thread when called. The callback takes one argument that it passes as a JSVal value to
     the Haskell function.

     Call 'releaseCallback' when done with the callback, freeing data referenced
     by the function.
 -}
syncCallback1 :: OnBlocked                             -- ^ what to do when the thread blocks
              -> (JSVal -> IO ())                      -- ^ the Haskell function
              -> IO (Callback (JSVal -> IO ()))        -- ^ the callback
syncCallback1 onBlocked x = js_syncCallbackApply (onBlocked == ContinueAsync) 1 (unsafeCoerce x)


{- | Make a callback (JavaScript function) that runs the supplied IO function in a synchronous
     thread when called. The callback takes two arguments that it passes as JSVal values to
     the Haskell function.

     Call 'releaseCallback' when done with the callback, freeing data referenced
     by the function.
 -}
syncCallback2 :: OnBlocked                               -- ^ what to do when the thread blocks
              -> (JSVal -> JSVal -> IO ())               -- ^ the Haskell function
              -> IO (Callback (JSVal -> JSVal -> IO ())) -- ^ the callback
syncCallback2 onBlocked x = js_syncCallbackApply (onBlocked == ContinueAsync) 2 (unsafeCoerce x)

{- | Make a callback (JavaScript function) that runs the supplied IO function in a synchronous
     thread when called. The callback takes three arguments that it passes as JSVal values to
     the Haskell function.

     Call 'releaseCallback' when done with the callback, freeing data referenced
     by the function.
 -}
syncCallback3 :: OnBlocked                               -- ^ what to do when the thread blocks
              -> (JSVal -> JSVal -> JSVal -> IO ())               -- ^ the Haskell function
              -> IO (Callback (JSVal -> JSVal -> JSVal -> IO ())) -- ^ the callback
syncCallback3 onBlocked x = js_syncCallbackApply (onBlocked == ContinueAsync) 3 (unsafeCoerce x)

{- | Make a callback (JavaScript function) that runs the supplied IO action in a synchronous
     thread when called.

     Call 'releaseCallback' when done with the callback, freeing memory referenced
     by the IO action.
 -}
syncCallback' :: IO JSVal
              -> IO (Callback (IO JSVal))
syncCallback' x = js_syncCallbackReturn (unsafeCoerce x)

syncCallback1' :: (JSVal -> IO JSVal)
               -> IO (Callback (JSVal -> IO JSVal))
syncCallback1' x = js_syncCallbackApplyReturn 1 (unsafeCoerce x)

syncCallback2' :: (JSVal -> JSVal -> IO JSVal)
               -> IO (Callback (JSVal -> JSVal -> IO JSVal))
syncCallback2' x = js_syncCallbackApplyReturn 2 (unsafeCoerce x)

syncCallback3' :: (JSVal -> JSVal -> JSVal -> IO JSVal)
               -> IO (Callback (JSVal -> JSVal -> JSVal -> IO JSVal))
syncCallback3' x = js_syncCallbackApplyReturn 3 (unsafeCoerce x)

{- | Make a callback (JavaScript function) that runs the supplied IO action in an asynchronous
     thread when called.

     Call 'releaseCallback' when done with the callback, freeing data referenced
     by the IO action.
 -}
asyncCallback :: IO ()              -- ^ the action that the callback runs
              -> IO (Callback (IO ())) -- ^ the callback
asyncCallback x = js_asyncCallback (unsafeCoerce x)

asyncCallback1 :: (JSVal -> IO ())            -- ^ the function that the callback calls
               -> IO (Callback (JSVal -> IO ())) -- ^ the calback
asyncCallback1 x = js_asyncCallbackApply 1 (unsafeCoerce x)

asyncCallback2 :: (JSVal -> JSVal -> IO ())            -- ^ the Haskell function that the callback calls
               -> IO (Callback (JSVal -> JSVal -> IO ())) -- ^ the callback
asyncCallback2 x = js_asyncCallbackApply 2 (unsafeCoerce x)

asyncCallback3 :: (JSVal -> JSVal -> JSVal -> IO ())               -- ^ the Haskell function that the callback calls
               -> IO (Callback (JSVal -> JSVal -> JSVal -> IO ())) -- ^ the callback
asyncCallback3 x = js_asyncCallbackApply 3 (unsafeCoerce x)

-- ----------------------------------------------------------------------------

js_syncCallback :: Bool -> Exts.Any -> IO (Callback (IO b))
js_syncCallback = stub

js_asyncCallback :: Exts.Any -> IO (Callback (IO b))
js_asyncCallback = stub

js_syncCallbackReturn :: Exts.Any -> IO (Callback (IO JSVal))
js_syncCallbackReturn = stub

js_syncCallbackApply :: Bool -> Int -> Exts.Any -> IO (Callback b)
js_syncCallbackApply = stub

js_asyncCallbackApply :: Int -> Exts.Any -> IO (Callback b)
js_asyncCallbackApply = stub

js_syncCallbackApplyReturn :: Int -> Exts.Any -> IO (Callback b)
js_syncCallbackApplyReturn = stub

js_release :: Callback a -> IO ()
js_release = stub




