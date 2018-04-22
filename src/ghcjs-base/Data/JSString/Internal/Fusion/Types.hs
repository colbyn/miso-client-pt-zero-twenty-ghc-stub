{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Data.Text.Internal.Fusion.Types
-- Copyright   : (c) Tom Harper 2008-2009,
--               (c) Bryan O'Sullivan 2009,
--               (c) Duncan Coutts 2009,
--               (c) Jasper Van der Jeugt 2011
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Core stream fusion functionality for text.
module Data.JSString.Internal.Fusion.Types where


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
import Data.Word (Word8)


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~



-- | Specialised tuple for case conversion.
data CC s = CC !s {-# UNPACK #-} !Char {-# UNPACK #-} !Char

-- | Specialised, strict Maybe-like type.
data M a = N
         | J !a

type M8 = M Word8

-- Restreaming state.
data RS s
    = RS0 !s
    | RS1 !s {-# UNPACK #-} !Word8
    | RS2 !s {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    | RS3 !s {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8

infixl 2 :*:
data PairS a b = !a :*: !b
                 -- deriving (Eq, Ord, Show)

-- | Allow a function over a stream to switch between two states.
data Switch = S1 | S2

data Step s a = Done
              | Skip !s
              | Yield !a !s

{-
instance (Show a) => Show (Step s a)
    where show Done        = "Done"
          show (Skip _)    = "Skip"
          show (Yield x _) = "Yield " ++ show x
-}

instance (Eq a) => Eq (Stream a) where
    (==) = eq

instance (Ord a) => Ord (Stream a) where
    compare = cmp

-- The length hint in a Stream has two roles.  If its value is zero,
-- we trust it, and treat the stream as empty.  Otherwise, we treat it
-- as a hint: it should usually be accurate, so we use it when
-- unstreaming to decide what size array to allocate.  However, the
-- unstreaming functions must be able to cope with the hint being too
-- small or too large.

data Stream a =
    forall s. Stream
    (s -> Step s a)             -- stepper function
    !s                          -- current state

-- | /O(n)/ Determines if two streams are equal.
eq :: (Eq a) => Stream a -> Stream a -> Bool
eq (Stream next1 s1) (Stream next2 s2) = loop (next1 s1) (next2 s2)
    where
      loop Done Done                     = True
      loop (Skip s1')     (Skip s2')     = loop (next1 s1') (next2 s2')
      loop (Skip s1')     x2             = loop (next1 s1') x2
      loop x1             (Skip s2')     = loop x1          (next2 s2')
      loop Done _                        = False
      loop _    Done                     = False
      loop (Yield x1 s1') (Yield x2 s2') = x1 == x2 &&
                                           loop (next1 s1') (next2 s2')
{-# INLINE [0] eq #-}

cmp :: (Ord a) => Stream a -> Stream a -> Ordering
cmp (Stream next1 s1) (Stream next2 s2) = loop (next1 s1) (next2 s2)
    where
      loop Done Done                     = EQ
      loop (Skip s1')     (Skip s2')     = loop (next1 s1') (next2 s2')
      loop (Skip s1')     x2             = loop (next1 s1') x2
      loop x1             (Skip s2')     = loop x1          (next2 s2')
      loop Done _                        = LT
      loop _    Done                     = GT
      loop (Yield x1 s1') (Yield x2 s2') =
          case compare x1 x2 of
            EQ    -> loop (next1 s1') (next2 s2')
            other -> other
{-# INLINE [0] cmp #-}

-- | The empty stream.
empty :: Stream a
empty = Stream next ()
    where next _ = Done
{-# INLINE [0] empty #-}