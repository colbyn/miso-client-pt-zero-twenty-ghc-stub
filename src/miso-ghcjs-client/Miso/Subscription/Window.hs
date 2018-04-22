{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Window
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.Window where


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
    , (<*>)
    , (=<<)
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
import Control.Monad
import Data.Monoid

import GHCJS.Foreign.Callback
import GHCJS.Marshal
import JavaScript.Object
import JavaScript.Object.Internal

import Miso.Event
import Miso.FFI
import Miso.Html.Internal         ( Sub )
import Miso.String

import Data.Aeson.Types (parseEither)


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


-- | Captures window coordinates changes as they occur and writes them to
-- an event sink
windowCoordsSub :: ((Int, Int) -> action) -> Sub action
windowCoordsSub f = \sink -> do
  sink . f =<< (,) <$> windowInnerHeight <*> windowInnerWidth
  windowAddEventListener "resize" =<< do
    asyncCallback1 $ \windowEvent -> do
      target <- getProp "target" (Object windowEvent)
      Just w <- fromJSVal =<< getProp "innerWidth" (Object target)
      Just h <- fromJSVal =<< getProp "innerHeight" (Object target)
      sink $ f (h, w)

-- | @windowOn eventName decoder toAction@ is a subscription which parallels the
-- attribute handler `on`, providing a subscription to listen to window level events.
windowSub :: MisoString -> Decoder r -> (r -> action) -> Sub action
windowSub  = windowSubWithOptions defaultOptions

windowSubWithOptions :: Options -> MisoString -> Decoder r -> (r -> action) -> Sub action
windowSubWithOptions Options{..} eventName Decoder{..} toAction = \sink -> do
  windowAddEventListener eventName =<< do
    decodeAtVal <- toJSVal decodeAt
    asyncCallback1 $ \e -> do
      Just v <- jsvalToValue =<< objectToJSON decodeAtVal e
      case parseEither decoder v of
        Left s -> error $ "Parse error on " <> unpack eventName <> ": " <> s
        Right r -> do
          when stopPropagation $ eventStopPropagation e
          when preventDefault $ eventPreventDefault e
          sink (toAction r)




