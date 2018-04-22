{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI
    ( windowAddEventListener
   , windowRemoveEventListener
   , windowInnerHeight
   , windowInnerWidth
   , eventPreventDefault
   , eventStopPropagation
   , now
   , consoleLog
   , stringify
   , parse
   , item
   , jsvalToValue
   , clearBody
   , objectToJSON
   , getWindow
   , set
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
    , toInteger
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
-- import qualified Data.Vector.Unboxed          as V
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
import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.Aeson                 as AE
import           Data.Aeson                 hiding (Object)
import qualified Data.HashMap.Strict        as H
import           Data.JSString
import qualified Data.JSString.Text         as JSS
import           Data.Maybe
import           Data.Scientific
import qualified Data.Vector                as V
import           GHCJS.Foreign.Callback
import           GHCJS.Foreign.Internal
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Array.Internal
import qualified JavaScript.Object.Internal as OI
import           Unsafe.Coerce


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~



-- | Set property on object
set :: ToJSVal v => JSString -> v -> OI.Object -> IO ()
set k v obj = toJSVal v >>= \x -> OI.setProp k x obj

-- | Convert JSVal to Maybe `Value`
jsvalToValue :: JSVal -> IO (Maybe Value)
jsvalToValue r = do
  case jsonTypeOf r of
    JSONNull -> return (Just Null)
    JSONInteger -> liftM (AE.Number . flip scientific 0 . (toInteger :: Int -> Integer))
         <$> fromJSVal r
    JSONFloat -> liftM (AE.Number . (fromFloatDigits :: Double -> Scientific))
         <$> fromJSVal r
    JSONBool -> liftM AE.Bool <$> fromJSVal r
    JSONString -> liftM AE.String <$> fromJSVal r
    JSONArray -> do
      xs :: [Value] <-
        catMaybes <$>
          forM (toList (unsafeCoerce r)) jsvalToValue
      pure . pure $ Array . V.fromList $ xs
    JSONObject -> do
        Just (props :: [JSString]) <- fromJSVal =<< getKeys (OI.Object r)
        runMaybeT $ do
            propVals <- forM props $ \p -> do
              v <- MaybeT (jsvalToValue =<< OI.getProp p (OI.Object r))
              return (JSS.textFromJSString p, v)
            return (AE.Object (H.fromList propVals))

-- | Retrieves keys
getKeys :: OI.Object -> IO JSVal
getKeys = stub

-- | Adds event listener to window
windowAddEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()
windowAddEventListener = stub

-- | Removes event listener from window
windowRemoveEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()
windowRemoveEventListener = stub


eventStopPropagation :: JSVal -> IO ()
eventStopPropagation = stub

eventPreventDefault :: JSVal -> IO ()
eventPreventDefault = stub


-- | Window object
getWindow :: IO JSVal
getWindow = stub


-- | Retrieves inner height
windowInnerHeight :: IO Int
windowInnerHeight = stub

-- | Retrieves outer height
windowInnerWidth :: IO Int
windowInnerWidth = stub

-- | Retrieve high performance time stamp
now :: IO Double
now = stub

-- | Console-logging
consoleLog :: JSVal -> IO ()
consoleLog = stub

-- | Converts a JS object into a JSON string
stringify' :: JSVal -> IO JSString
stringify' = stub

parse' :: JSVal -> IO JSVal
parse' = stub

-- | Converts a JS object into a JSON string
stringify :: ToJSON json => json -> IO JSString
{-# INLINE stringify #-}
stringify j = stringify' =<< toJSVal (toJSON j)

-- | Parses a JSString
parse :: FromJSON json => JSVal -> IO json
{-# INLINE parse #-}
parse jval = do
  k <- parse' jval
  Just val <- jsvalToValue k
  case fromJSON val of
    Success x -> pure x
    Error y -> error y

-- | Indexing into a JS object
item :: JSVal -> JSString -> IO JSVal
item = stub

-- | Clear the document body. This is particularly useful to avoid
-- creating multiple copies of your app when running in GHCJSi.
clearBody :: IO ()
clearBody = stub


objectToJSON
    :: JSVal -- ^ decodeAt :: [JSString]
    -> JSVal -- ^ object with impure references to the DOM
    -> IO JSVal
objectToJSON = stub



