{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI,
             UnliftedFFITypes, DeriveDataTypeable, MagicHash
#-}


{- | GHCJS has two types of threads. Regular, asynchronous threads are
     started with `h$run`, are managed by the scheduler and run in the
     background. `h$run` returns immediately.

     Synchronous threads are started with `h$runSync`, which returns
     when the thread has run to completion. When a synchronous thread
     does an operation that would block, like accessing an MVar or
     an asynchronous FFI call, it cannot continue synchronously.

     There are two ways this can be resolved, depending on the
     second argument of the `h$runSync` call:

      * The action is aborted and the thread receives a 'WouldBlockException'
      * The thread continues asynchronously, `h$runSync` returns

     Note: when a synchronous thread encounters a black hole from
     another thread, it tries to steal the work from that thread
     to avoid blocking. In some cases that might not be possible,
     for example when the data accessed is produced by a lazy IO
     operation. This is resolved the same way as blocking on an IO
     action would be.
 -}
module GHCJS.Concurrent (
    isThreadSynchronous
  , isThreadContinueAsync
  , OnBlocked(..)
  , WouldBlockException(..)
  , withoutPreemption
  , synchronously
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
    , Enum
    , fmap
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
import           GHCJS.Prim

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception as Ex

import           GHC.Exts (ThreadId#)
import           GHC.Conc.Sync (ThreadId(..))

import           Data.Bits (testBit)
import           Data.Data
import           Data.Typeable

import           Unsafe.Coerce


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~



{- |
     The runtime tries to run synchronous threads to completion. Sometimes it's
     not possible to continue running a thread, for example when the thread
     tries to take an empty 'MVar'. The runtime can then either throw a
     'WouldBlockException', aborting the blocking action, or continue the
     thread asynchronously.
 -}

data OnBlocked = ContinueAsync -- ^ continue the thread asynchronously if blocked
               | ThrowWouldBlock -- ^ throw 'WouldBlockException' if blocked
               deriving (Data, Typeable, Enum, Show, Eq, Ord)

{- |
     Run the action without the scheduler preempting the thread. When a blocking
     action is encountered, the thread is still suspended and will continue
     without preemption when it's woken up again.

     When the thread encounters a black hole from another thread, the scheduler
     will attempt to clear it by temporarily switching to that thread.
 -}

withoutPreemption :: IO a -> IO a
withoutPreemption x = Ex.mask $ \restore -> do
  oldS <- js_setNoPreemption True
  if oldS
    then restore x
    else restore x `Ex.finally` js_setNoPreemption False
{-# INLINE withoutPreemption #-}


{- |
     Run the action synchronously, which means that the thread will not
     be preempted by the scheduler. If the thread encounters a blocking
     operation, the runtime throws a WouldBlock exception.

     When the thread encounters a black hole from another thread, the scheduler
     will attempt to clear it by temporarily switching to that thread.
 -}
synchronously :: IO a -> IO a
synchronously x = Ex.mask $ \restore -> do
  oldS <- js_setSynchronous True
  if oldS
    then restore x
    else restore x `Ex.finally` js_setSynchronous False
{-# INLINE synchronously #-}

{- | Returns whether the 'ThreadId' is a synchronous thread
 -}
isThreadSynchronous :: ThreadId -> IO Bool
isThreadSynchronous = fmap (`testBit` 0) . syncThreadState

{- |
     Returns whether the 'ThreadId' will continue running async. Always
     returns 'True' when the thread is not synchronous.
 -}
isThreadContinueAsync :: ThreadId -> IO Bool
isThreadContinueAsync = fmap (`testBit` 1) . syncThreadState

{- |
     Returns whether the 'ThreadId' is not preemptible. Always
     returns 'True' when the thread is synchronous.
 -}
isThreadNonPreemptible :: ThreadId -> IO Bool
isThreadNonPreemptible = fmap (`testBit` 2) . syncThreadState

syncThreadState :: ThreadId-> IO Int
syncThreadState (ThreadId tid) = js_syncThreadState tid

-- ----------------------------------------------------------------------------


js_syncThreadState :: ThreadId# -> IO Int
js_syncThreadState = stub
js_setNoPreemption :: Bool -> IO Bool;
js_setNoPreemption = stub
js_setSynchronous :: Bool -> IO Bool
js_setSynchronous = stub



