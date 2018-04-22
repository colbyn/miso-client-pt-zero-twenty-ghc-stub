{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Util
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Util
    ( withFoldable
    , conditionalViews
    , (=:)
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
-- Miso Specific
-- ---------------
import Data.Foldable
import qualified Data.Map as M
import Miso.Html (View)


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


-- | Generic @map@ function, useful for creating @View@s from the elements of
-- some @Foldable@. Particularly handy for @Maybe@, as shown in the example
-- below.
--
-- @
-- view model =
--     div_ [] $
--      withFoldable (model ^. mSomeMaybeVal) $ \someVal ->
--         p_ [] [ text $ "Hey, look at this value: " <> ms (show someVal) ]
-- @
withFoldable :: Foldable t => t a -> (a -> b) -> [b]
withFoldable ta f = map f (toList ta)

-- | Hides the @View@s the condition is False. Shows them when the condition
-- is True.
conditionalViews :: Bool -> [View action] -> [View action]
conditionalViews condition views =
    if condition
    then views
    else []

-- | Smart constructor for Attributes. This function is helpful when constructing numerous Attributes
-- Example shown below.
-- 
-- @ 
-- div_ [ style_  $ ("background" =: "red" <> "width" =: "250px" <> "height" =: "250px") ] []
-- @
(=:) :: k -> a -> M.Map k a 
a =: b = M.singleton a b


