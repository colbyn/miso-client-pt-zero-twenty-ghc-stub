{-# LANGUAGE NoImplicitPrelude #-}

-- | Haskell-specific web worker API. The URL is expected to point to a script
--      that is the same as the caller, or at least a script that has been
--      produced by GHCJS and contains the same static values.
--
-- NOTE (Stub Autor):
-- This doesn’t seem to be publicly exported, or used… I.e. this module is old, dead code.
-- Exports have been commend out.

module JavaScript.Web.Worker.Haskell
    -- ( HaskellWorker
    -- , terminate
    -- , call
    -- )
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
import qualified JavaScript.Web.Worker as W


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~

-- 
-- data HaskellWorker = HaskellWorker W.Worker
-- 
-- create :: JSString -> IO HaskellWorker
-- create uri = fmap HaskellWorker (W.create uri)
-- {-# INLINE create #-}
-- 
-- -- fixme stop all waiters?
-- terminate :: HaskellWorker -> IO ()
-- terminate (HaskellWorker w) = W.terminate w
-- {-# INLINE terminate #-}
-- 
-- -- call :: SomethingSomething -> HaskellWorker -> IO a
-- call hw = undefined
-- {-# INLINE call #-}
-- 
-- 
-- 
