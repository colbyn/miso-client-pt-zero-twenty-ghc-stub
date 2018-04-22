{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.String
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.String
    (
    ToMisoString (..)
    , MisoString
    , module Data.JSString
    , module Data.Monoid
    , ms
    )
where


-- ~
import Core hiding (pack)
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
    , Word(..)
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
import           Data.Aeson
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import           Data.JSString
import           Data.JSString.Int
import           Data.JSString.RealFloat
import           Data.JSString.Text
import           Data.Monoid
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import           GHCJS.Marshal.Pure
import           GHCJS.Types



-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~



-- | String type swappable based on compiler
type MisoString = JSString

-- | `ToJSON` for `MisoString`
instance ToJSON MisoString where
  toJSON = String . textFromJSString

-- | `FromJSON` for `MisoString`
instance FromJSON MisoString where
  parseJSON =
    withText "Not a valid string" $ \x ->
      pure (toMisoString x)

-- | Convenience class for creating `MisoString` from other string-like types
class ToMisoString str where
  toMisoString :: str -> MisoString
  fromMisoString :: MisoString -> str

-- | Convenience function, shorthand for `toMisoString`
ms :: ToMisoString str => str -> MisoString
ms = toMisoString

instance ToMisoString MisoString where
  toMisoString = id
  fromMisoString = id
instance ToMisoString String where
  toMisoString = pack
  fromMisoString = unpack
instance ToMisoString T.Text where
  toMisoString = textToJSString
  fromMisoString = textFromJSString
instance ToMisoString LT.Text where
  toMisoString = lazyTextToJSString
  fromMisoString = lazyTextFromJSString
instance ToMisoString B.ByteString where
  toMisoString = toMisoString . T.decodeUtf8
  fromMisoString = T.encodeUtf8 . fromMisoString
instance ToMisoString BL.ByteString where
  toMisoString = toMisoString . LT.decodeUtf8
  fromMisoString = LT.encodeUtf8 . fromMisoString
instance ToMisoString Float where
  toMisoString = realFloat
  fromMisoString = pFromJSVal . toJSNumber
instance ToMisoString Double where
  toMisoString = realFloat
  fromMisoString = pFromJSVal . toJSNumber
instance ToMisoString Int where
  toMisoString = decimal
  fromMisoString = pFromJSVal . toJSNumber
instance ToMisoString Word where
  toMisoString = decimal
  fromMisoString = pFromJSVal . toJSNumber

toJSNumber :: JSString -> JSVal
toJSNumber = stub





