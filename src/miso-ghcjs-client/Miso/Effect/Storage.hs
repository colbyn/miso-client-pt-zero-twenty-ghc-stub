{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE LambdaCase                #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Effect.Storage
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides an interface to the
-- [Web Storage API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API).
----------------------------------------------------------------------------
module Miso.Effect.Storage
    ( -- * Retrieve storage
    getLocalStorage
    , getSessionStorage
    -- * Set items in storage
    , setLocalStorage
    , setSessionStorage
    -- * Remove items from storage
    , removeLocalStorage
    , removeSessionStorage
    -- * Clear storage
    , clearLocalStorage
    , clearSessionStorage
    -- * Get number of items in storage
    , localStorageLength
    , sessionStorageLength
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
import Data.Aeson     hiding (Object, String)
import Data.JSString
import GHCJS.Nullable
import GHCJS.Types

import Miso.FFI


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~



-- | Helper for retrieving either local or session storage
getStorageCommon
  :: FromJSON b => (t -> IO (Maybe JSVal)) -> t -> IO (Either String b)
getStorageCommon f key = do
  result :: Maybe JSVal <- f key
  case result of
    Nothing -> pure $ Left "Not Found"
    Just v -> do
      r <- parse v
      pure $ case fromJSON r of
        Success x -> Right x
        Error y -> Left y

-- | Retrieve session storage
getSessionStorage :: FromJSON model => JSString -> IO (Either String model)
getSessionStorage =
  getStorageCommon $ \t -> do
    r <- getItemSS t
    pure (nullableToMaybe r)

-- | Retrieve local storage
getLocalStorage :: FromJSON model => JSString -> IO (Either String model)
getLocalStorage = getStorageCommon $ \t -> do
    r <- getItemLS t
    pure (nullableToMaybe r)

-- | Set the value of a key in local storage.
--
-- @setLocalStorage key value@ sets the value of @key@ to @value@.
setLocalStorage :: ToJSON model => JSString -> model -> IO ()
setLocalStorage key model =
  setItemLS key =<< stringify model

-- | Set the value of a key in session storage.
--
-- @setSessionStorage key value@ sets the value of @key@ to @value@.
setSessionStorage :: ToJSON model => JSString -> model -> IO ()
setSessionStorage key model =
  setItemSS key =<< stringify model

getItemLS :: JSString -> IO (Nullable JSVal)
getItemLS = stub

getItemSS :: JSString -> IO (Nullable JSVal)
getItemSS = stub

-- | Removes item from local storage by key name.
removeLocalStorage :: JSString -> IO ()
removeLocalStorage = stub

-- | Removes item from session storage by key name.
removeSessionStorage :: JSString -> IO ()
removeSessionStorage = stub

setItemLS :: JSString -> JSString -> IO ()
setItemLS = stub

setItemSS :: JSString -> JSString -> IO ()
setItemSS = stub

-- | Retrieves the number of items in local storage.
localStorageLength :: IO Int
localStorageLength = stub

-- | Retrieves the number of items in session storage.
sessionStorageLength :: IO Int
sessionStorageLength = stub

-- | Clears local storage.
clearLocalStorage :: IO ()
clearLocalStorage = stub

-- | Clears session storage.
clearSessionStorage :: IO ()
clearSessionStorage = stub





