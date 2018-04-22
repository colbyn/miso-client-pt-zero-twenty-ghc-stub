{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Keyboard
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.Keyboard
    ( -- * Types
    Arrows (..)
    -- * Subscriptions
    , arrowsSub
    , directionSub
    , keyboardSub
    , wasdSub
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
    , (=<<)
    , any
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
import           Data.Set
import qualified Data.Set as S
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           JavaScript.Object
import           JavaScript.Object.Internal

import           Miso.FFI
import           Miso.Html.Internal ( Sub )


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~

-- | type for arrow keys currently pressed
--  37 left arrow  ( x = -1 )
--  38 up arrow    ( y =  1 )
--  39 right arrow ( x =  1 )
--  40 down arrow  ( y = -1 )
data Arrows = Arrows {
   arrowX :: !Int
 , arrowY :: !Int
 } deriving (Show, Eq)

-- | Helper function to convert keys currently pressed to `Arrow`, given a
-- mapping for keys representing up, down, left and right respectively.
toArrows :: ([Int], [Int], [Int], [Int]) -> Set Int -> Arrows
toArrows (up, down, left, right) set' =
  Arrows {
    arrowX =
      case (check left, check right) of
        (True, False) -> -1
        (False, True) -> 1
        (_,_) -> 0
  , arrowY =
      case (check down, check up) of
        (True, False) -> -1
        (False, True) -> 1
        (_,_) -> 0
  }
  where
    check = any (`S.member` set')

-- | Maps `Arrows` onto a Keyboard subscription
arrowsSub :: (Arrows -> action) -> Sub action
arrowsSub = directionSub ([38], [40], [37], [39])

-- | Maps `WASD` onto a Keyboard subscription for directions
wasdSub :: (Arrows -> action) -> Sub action
wasdSub = directionSub ([87], [83], [65], [68])

-- | Maps a specified list of keys to directions (up, down, left, right)
directionSub :: ([Int], [Int], [Int], [Int])
             -> (Arrows -> action)
             -> Sub action
directionSub dirs = keyboardSub . (. toArrows dirs)

-- | Returns subscription for Keyboard
keyboardSub :: (Set Int -> action) -> Sub action
keyboardSub f sink = do
  keySetRef <- newIORef mempty
  windowAddEventListener "keyup" =<< keyUpCallback keySetRef
  windowAddEventListener "keydown" =<< keyDownCallback keySetRef
    where
      keyDownCallback keySetRef = do
        asyncCallback1 $ \keyDownEvent -> do
          Just key <- fromJSVal =<< getProp "keyCode" (Object keyDownEvent)
          newKeys <- atomicModifyIORef' keySetRef $ \keys ->
             let !new = S.insert key keys
             in (new, new)
          sink (f newKeys)

      keyUpCallback keySetRef = do
        asyncCallback1 $ \keyUpEvent -> do
          Just key <- fromJSVal =<< getProp "keyCode" (Object keyUpEvent)
          newKeys <- atomicModifyIORef' keySetRef $ \keys ->
             let !new = S.delete key keys
             in (new, new)
          sink (f newKeys)




