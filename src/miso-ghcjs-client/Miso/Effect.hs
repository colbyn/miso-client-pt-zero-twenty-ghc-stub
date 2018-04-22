{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Effect
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Effect
    ( module Miso.Effect.Storage
    , module Miso.Effect.XHR
    , module Miso.Effect.DOM
    , Effect (..), Sub, Sink
    , mapSub
    , noEff
    , (<#)
    , (#>)
    , batchEff
    , effectSub
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
    , Functor(..)
    , Applicative(..)
    , Monad(..)
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
import Data.Bifunctor

import Miso.Effect.Storage
import Miso.Effect.XHR
import Miso.Effect.DOM


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


-- | An effect represents the results of an update action.
--
-- It consists of the updated model and a list of subscriptions. Each 'Sub' is
-- run in a new thread so there is no risk of accidentally blocking the
-- application.
data Effect action model = Effect model [Sub action]

-- | Type synonym for constructing event subscriptions.
--
-- The 'Sink' callback is used to dispatch actions which are then fed
-- back to the 'update' function.
type Sub action = Sink action -> IO ()

-- | Function to asynchronously dispatch actions to the 'update' function.
type Sink action = action -> IO ()

-- | Turn a subscription that consumes actions of type @a@ into a subscription
-- that consumes actions of type @b@ using the supplied function of type @a -> b@.
mapSub :: (actionA -> actionB) -> Sub actionA -> Sub actionB
mapSub f sub = \sinkB -> let sinkA = sinkB . f
                         in sub sinkA

instance Functor (Effect action) where
  fmap f (Effect m acts) = Effect (f m) acts

instance Applicative (Effect action) where
  pure m = Effect m []
  Effect fModel fActs <*> Effect xModel xActs = Effect (fModel xModel) (fActs ++ xActs)

instance Monad (Effect action) where
  return = pure
  Effect m acts >>= f =
    case f m of
      Effect m' acts' -> Effect m' (acts ++ acts')

instance Bifunctor Effect where
  bimap f g (Effect m acts) = Effect (g m) (map (\act -> \sink -> act (sink . f)) acts)

-- | Smart constructor for an 'Effect' with no actions.
noEff :: model -> Effect action model
noEff m = Effect m []

-- | Smart constructor for an 'Effect' with exactly one action.
(<#) :: model -> IO action -> Effect action model
(<#) m a = effectSub m $ \sink -> a >>= sink

-- | `Effect` smart constructor, flipped
(#>) :: IO action -> model -> Effect action model
(#>) = flip (<#)

-- | `Smart constructor for an 'Effect' with multiple actions.
batchEff :: model -> [IO action] -> Effect action model
batchEff model actions = Effect model $ (>>=) <$> actions

-- | Like '<#' but schedules a subscription which is an IO computation which has
-- access to a 'Sink' which can be used to asynchronously dispatch actions to
-- the 'update' function.
--
-- A use-case is scheduling an IO computation which creates a 3rd-party JS
-- widget which has an associated callback. The callback can then call the sink
-- to turn events into actions. To do this without accessing a sink requires
-- going via a @'Sub'scription@ which introduces a leaky-abstraction.
effectSub :: model -> Sub action -> Effect action model
effectSub model sub = Effect model [sub]




