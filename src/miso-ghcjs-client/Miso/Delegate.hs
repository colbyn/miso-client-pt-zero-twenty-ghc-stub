{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Delegate
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Delegate where


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
-- Miso Specific
-- ---------------
import           Data.IORef
import qualified Data.Map                 as M
import           Miso.Html.Internal
import           Miso.String
import qualified JavaScript.Object.Internal as OI
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types (JSVal)


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


-- | Entry point for event delegation
delegator
  :: JSVal
  -> IORef VTree
  -> M.Map MisoString Bool
  -> IO ()
delegator mountPointElement vtreeRef es = do
  evts <- toJSVal (M.toList es)
  getVTreeFromRef <- syncCallback' $ do
    VTree (OI.Object val) <- readIORef vtreeRef
    pure val
  delegateEvent mountPointElement evts getVTreeFromRef

-- | Event delegation FFI, routes events received on body through the virtual dom
-- Invokes event handler when found
delegateEvent
     :: JSVal               -- ^ mountPoint element
     -> JSVal               -- ^ Events
     -> Callback (IO JSVal) -- ^ Virtual DOM callback
     -> IO ()

delegateEvent = stub


