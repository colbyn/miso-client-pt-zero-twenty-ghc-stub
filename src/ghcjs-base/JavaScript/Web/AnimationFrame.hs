{-# LANGUAGE NoImplicitPrelude #-}


{-# LANGUAGE DeriveDataTypeable #-}

{- |
     Animation frames are the browser's mechanism for smooth animation.
     An animation frame callback is run just before the browser repaints.

     When the content window is inactive, for example when the user is looking
     at another tab, it can take a long time for an animation frame callback
     to happen. Be careful structuring evaluation around this! Typically this
     means carefully forcing the data before the animation frame is requested,
     so the callback can run quickly and predictably.
  -}

module JavaScript.Web.AnimationFrame
    ( waitForAnimationFrame
    , inAnimationFrame
    , cancelAnimationFrame
    , AnimationFrameHandle
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
import GHCJS.Foreign.Callback
import GHCJS.Marshal.Pure
import GHCJS.Types

import Control.Exception (onException)
import Data.Typeable


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


newtype AnimationFrameHandle = AnimationFrameHandle JSVal
  deriving (Typeable)

{- |
     Wait for an animation frame callback to continue running the current
     thread. Use 'GHCJS.Concurrent.synchronously' if the thread should
     not be preempted. This will return the high-performance clock time
     stamp once an animation frame is reached.
 -}
waitForAnimationFrame :: IO Double
waitForAnimationFrame = do
  h <- js_makeAnimationFrameHandle
  js_waitForAnimationFrame h `onException` js_cancelAnimationFrame h

{- |
     Run the action in an animationframe callback. The action runs in a
     synchronous thread, and is passed the high-performance clock time
     stamp for that frame.
 -}
inAnimationFrame :: OnBlocked       -- ^ what to do when encountering a blocking call
                 -> (Double -> IO ())  -- ^ the action to run
                 -> IO AnimationFrameHandle
inAnimationFrame onBlocked x = do
  cb <- syncCallback1 onBlocked (x . pFromJSVal)
  h  <- js_makeAnimationFrameHandleCallback (jsval cb)
  js_requestAnimationFrame h
  return h

cancelAnimationFrame :: AnimationFrameHandle -> IO ()
cancelAnimationFrame h = js_cancelAnimationFrame h
{-# INLINE cancelAnimationFrame #-}

-- -----------------------------------------------------------------------------

js_makeAnimationFrameHandle :: IO AnimationFrameHandle
js_makeAnimationFrameHandle = stub

js_makeAnimationFrameHandleCallback :: JSVal -> IO AnimationFrameHandle
js_makeAnimationFrameHandleCallback = stub

js_cancelAnimationFrame :: AnimationFrameHandle -> IO ()
js_cancelAnimationFrame = stub

js_waitForAnimationFrame :: AnimationFrameHandle -> IO Double
js_waitForAnimationFrame = stub

js_requestAnimationFrame :: AnimationFrameHandle -> IO ()
js_requestAnimationFrame = stub





