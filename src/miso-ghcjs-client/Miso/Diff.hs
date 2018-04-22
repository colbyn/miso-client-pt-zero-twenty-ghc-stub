{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Diff
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Diff
    ( diff
    , mountElement
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
-- Miso Specific
-- ---------------
import GHCJS.Foreign.Internal     hiding (Object)
import GHCJS.Types
import JavaScript.Object
import JavaScript.Object.Internal
import Miso.Html.Internal



-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


-- | Entry point for diffing / patching algorithm
diff :: Maybe JSString -> Maybe VTree -> Maybe VTree -> IO ()
diff mayElem current new =
  case mayElem of
    Nothing -> do
      body <- getBody
      diffElement body current new
    Just elemId -> do
      e <- getElementById elemId
      diffElement e current new

-- | diffing / patching a given element
diffElement :: JSVal -> Maybe VTree -> Maybe VTree -> IO ()
diffElement mountEl current new = do
  doc <- getDoc
  case (current, new) of
    (Nothing, Nothing) -> pure ()
    (Just (VTree current'), Just (VTree new')) -> do
      diff' current' new' mountEl doc
    (Nothing, Just (VTree new')) -> do
      diff' (Object jsNull) new' mountEl doc
    (Just (VTree current'), Nothing) -> do
      diff' current' (Object jsNull) mountEl doc

-- | return the configured mountPoint element or the body
mountElement :: Maybe JSString -> IO JSVal
mountElement mayMp =
  case mayMp of
    Nothing -> getBody
    Just eid -> getElementById eid

getBody :: IO JSVal
getBody = stub

getDoc :: IO JSVal
getDoc = stub

getElementById :: JSString -> IO JSVal
getElementById = stub

diff'
    :: Object -- ^ current object
    -> Object -- ^ new object
    -> JSVal  -- ^ parent node
    -> JSVal  -- ^ document
    -> IO ()

diff' = stub

