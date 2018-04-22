{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.WebSocket
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.WebSocket
    ( -- * Types
    WebSocket   (..)
    , URL         (..)
    , Protocols   (..)
    , SocketState (..)
    , CloseCode   (..)
    , WasClean    (..)
    , Reason      (..)
    -- * Subscription
    , websocketSub
    , send
    , connect
    , getSocketState
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
import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.IORef
import Data.Maybe
import GHC.Generics
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types
import Prelude                hiding (map)
import System.IO.Unsafe

import Miso.FFI
import Miso.Html.Internal     ( Sub )
import Miso.String


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~



-- | WebSocket connection messages
data WebSocket action
  = WebSocketMessage action
  | WebSocketClose CloseCode WasClean Reason
  | WebSocketOpen
  | WebSocketError MisoString
    deriving (Show, Eq)

websocket :: IORef (Maybe Socket)
{-# NOINLINE websocket #-}
websocket = unsafePerformIO (newIORef Nothing)

closedCode :: IORef (Maybe CloseCode)
{-# NOINLINE closedCode #-}
closedCode = unsafePerformIO (newIORef Nothing)

secs :: Int -> Int
secs = (*1000000)

-- | WebSocket subscription
websocketSub
  :: FromJSON m
  => URL
  -> Protocols
  -> (WebSocket m -> action)
  -> Sub action
websocketSub (URL u) (Protocols ps) f sink = do
  socket <- createWebSocket u ps
  writeIORef websocket (Just socket)
  void . forkIO $ handleReconnect
  onOpen socket =<< do
    writeIORef closedCode Nothing
    asyncCallback $ sink (f WebSocketOpen)
  onMessage socket =<< do
    asyncCallback1 $ \v -> do
      d <- parse =<< getData v
      sink $ f (WebSocketMessage d)
  onClose socket =<< do
    asyncCallback1 $ \e -> do
      code <- codeToCloseCode <$> getCode e
      writeIORef closedCode (Just code)
      reason <- getReason e
      clean <- wasClean e
      sink $ f (WebSocketClose code clean reason)
  onError socket =<< do
    asyncCallback1 $ \v -> do
      writeIORef closedCode Nothing
      d <- parse =<< getData v
      sink $ f (WebSocketError d)
  where
    handleReconnect = do
      threadDelay (secs 3)
      Just s <- readIORef websocket
      status <- getSocketState' s
      code <- readIORef closedCode
      if status == 3
        then do
          unless (code == Just CLOSE_NORMAL) $
            websocketSub (URL u) (Protocols ps) f sink
        else handleReconnect

-- | Sends message to a websocket server
send :: ToJSON a => a -> IO ()
{-# INLINE send #-}
send x = do
  Just socket <- readIORef websocket
  sendJson' socket x

-- | Connects to a websocket server
connect :: URL -> Protocols -> IO ()
{-# INLINE connect #-}
connect (URL url') (Protocols ps) = do
  Just ws <- readIORef websocket
  s <- getSocketState' ws
  when (s == 3) $ do
    socket <- createWebSocket url' ps
    atomicWriteIORef websocket (Just socket)

-- | URL of Websocket server
newtype URL = URL MisoString
  deriving (Show, Eq)

-- | Protocols for Websocket connection
newtype Protocols = Protocols [MisoString]
  deriving (Show, Eq)

-- | Wether or not the connection closed was done so cleanly
newtype WasClean = WasClean Bool deriving (Show, Eq)

-- | Reason for closed connection
newtype Reason = Reason MisoString deriving (Show, Eq)

createWebSocket' :: JSString -> JSVal -> IO Socket
createWebSocket' = stub

getSocketState' :: Socket -> IO Int
getSocketState' = stub


-- | `SocketState` corresponding to current WebSocket connection
data SocketState
  = CONNECTING -- ^ 0
  | OPEN       -- ^ 1
  | CLOSING    -- ^ 2
  | CLOSED     -- ^ 3
  deriving (Show, Eq, Ord, Enum)

-- | Retrieves current status of `WebSocket`
getSocketState :: IO SocketState
getSocketState = do
  Just ws <- readIORef websocket
  toEnum <$> getSocketState' ws

send' :: Socket -> JSString -> IO ()
send' = stub

sendJson' :: ToJSON json => Socket -> json -> IO ()
sendJson' socket m = send' socket =<< stringify m

createWebSocket :: JSString -> [JSString] -> IO Socket
{-# INLINE createWebSocket #-}
createWebSocket url' protocols =
  createWebSocket' url' =<< toJSVal protocols

onOpen :: Socket -> Callback (IO ()) -> IO ()
onOpen = stub

onClose :: Socket -> Callback (JSVal -> IO ()) -> IO ()
onClose = stub

onMessage :: Socket -> Callback (JSVal -> IO ()) -> IO ()
onMessage = stub

onError :: Socket -> Callback (JSVal -> IO ()) -> IO ()
onError = stub

getData :: JSVal -> IO JSVal
getData = stub

wasClean :: JSVal -> IO WasClean
wasClean = stub

getCode :: JSVal -> IO Int
getCode = stub

getReason :: JSVal -> IO Reason
getReason = stub


newtype Socket = Socket JSVal

-- | Code corresponding to a closed connection
-- https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent
data CloseCode
  = CLOSE_NORMAL
   -- ^ 1000, Normal closure; the connection successfully completed whatever purpose for which it was created.
  | CLOSE_GOING_AWAY
   -- ^ 1001, The endpoint is going away, either because of a server failure or because the browser is navigating away from the page that opened the connection.
  | CLOSE_PROTOCOL_ERROR
   -- ^ 1002, The endpoint is terminating the connection due to a protocol error.
  | CLOSE_UNSUPPORTED
   -- ^ 1003, The connection is being terminated because the endpoint received data of a type it cannot accept (for example, a textonly endpoint received binary data).
  | CLOSE_NO_STATUS
   -- ^ 1005, Reserved.  Indicates that no status code was provided even though one was expected.
  | CLOSE_ABNORMAL
   -- ^ 1006, Reserved. Used to indicate that a connection was closed abnormally (that is, with no close frame being sent) when a status code is expected.
  | Unsupported_Data
   -- ^ 1007, The endpoint is terminating the connection because a message was received that contained inconsistent data (e.g., nonUTF8 data within a text message).
  | Policy_Violation
   -- ^ 1008, The endpoint is terminating the connection because it received a message that violates its policy. This is a generic status code, used when codes 1003 and 1009 are not suitable.
  | CLOSE_TOO_LARGE
   -- ^ 1009, The endpoint is terminating the connection because a data frame was received that is too large.
  | Missing_Extension
   -- ^ 1010, The client is terminating the connection because it expected the server to negotiate one or more extension, but the server didn't.
  | Internal_Error
   -- ^ 1011, The server is terminating the connection because it encountered an unexpected condition that prevented it from fulfilling the request.
  | Service_Restart
   -- ^ 1012, The server is terminating the connection because it is restarting.
  | Try_Again_Later
   -- ^ 1013, The server is terminating the connection due to a temporary condition, e.g. it is overloaded and is casting off some of its clients.
  | TLS_Handshake
   -- ^ 1015, Reserved. Indicates that the connection was closed due to a failure to perform a TLS handshake (e.g., the server certificate can't be verified).
  | OtherCode Int
   -- ^ OtherCode that is reserved and not in the range 0999
  deriving (Show, Eq, Generic)

instance ToJSVal CloseCode
instance FromJSVal CloseCode

codeToCloseCode :: Int -> CloseCode
codeToCloseCode = go
  where
    go 1000 = CLOSE_NORMAL
    go 1001 = CLOSE_GOING_AWAY
    go 1002 = CLOSE_PROTOCOL_ERROR
    go 1003 = CLOSE_UNSUPPORTED
    go 1005 = CLOSE_NO_STATUS
    go 1006 = CLOSE_ABNORMAL
    go 1007 = Unsupported_Data
    go 1008 = Policy_Violation
    go 1009 = CLOSE_TOO_LARGE
    go 1010 = Missing_Extension
    go 1011 = Internal_Error
    go 1012 = Service_Restart
    go 1013 = Try_Again_Later
    go 1015 = TLS_Handshake
    go n    = OtherCode n




