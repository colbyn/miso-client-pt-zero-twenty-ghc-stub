{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg.Events
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Svg.Event
    ( -- * Animation event handlers
    onBegin
    , onEnd
    , onRepeat
    -- * Document event attributes
    , onAbort
    , onError
    , onResize
    , onScroll
    , onLoad
    , onUnload
    , onZoom
    -- * Graphical Event Attributes
    , onActivate
    , onClick
    , onFocusIn
    , onFocusOut
    , onMouseDown
    , onMouseMove
    , onMouseOut
    , onMouseOver
    , onMouseUp
    )
where


-- ~
import Core hiding (on)
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
import Miso.Event 
import Miso.Html.Event (onClick)
import Miso.Html.Internal



-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~



-- | onBegin event
onBegin :: action -> Attribute action
onBegin action = on "begin" emptyDecoder $ \() -> action

-- | onEnd event
onEnd :: action -> Attribute action
onEnd action = on "end" emptyDecoder $ \() -> action

-- | onRepeat event
onRepeat :: action -> Attribute action
onRepeat action = on "repeat" emptyDecoder $ \() -> action

-- | onAbort event
onAbort :: action -> Attribute action
onAbort action = on "abort" emptyDecoder $ \() -> action

-- | onError event
onError :: action -> Attribute action
onError action = on "error" emptyDecoder $ \() -> action

-- | onResize event
onResize :: action -> Attribute action
onResize action = on "resize" emptyDecoder $ \() -> action

-- | onScroll event
onScroll :: action -> Attribute action
onScroll action = on "scroll" emptyDecoder $ \() -> action

-- | onLoad event
onLoad :: action -> Attribute action
onLoad action = on "load" emptyDecoder $ \() -> action

-- | onUnload event
onUnload :: action -> Attribute action
onUnload action = on "unload" emptyDecoder $ \() -> action

-- | onZoom event
onZoom :: action -> Attribute action
onZoom action = on "zoom" emptyDecoder $ \() -> action

-- | onActivate event
onActivate :: action -> Attribute action
onActivate action = on "activate" emptyDecoder $ \() -> action

-- | onFocusIn event
onFocusIn :: action -> Attribute action
onFocusIn action = on "focusin" emptyDecoder $ \() -> action

-- | onFocusOut event
onFocusOut :: action -> Attribute action
onFocusOut action = on "focusout" emptyDecoder $ \() -> action

-- | onMouseDown event
onMouseDown :: action -> Attribute action
onMouseDown action = on "mousedown" emptyDecoder $ \() -> action
-- | onMouseMove event
onMouseMove :: action -> Attribute action
onMouseMove action = on "mousemove" emptyDecoder $ \() -> action

-- | onMouseOut event
onMouseOut :: action -> Attribute action
onMouseOut action = on "mouseout" emptyDecoder $ \() -> action

-- | onMouseOver event
onMouseOver :: action -> Attribute action
onMouseOver action = on "mouseover" emptyDecoder $ \() -> action

-- | onMouseUp event
onMouseUp :: action -> Attribute action
onMouseUp action = on "mouseup" emptyDecoder $ \() -> action






