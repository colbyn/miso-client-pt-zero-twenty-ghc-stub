{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnboxedTuples #-}
module JavaScript.Web.WebSocket
    ( WebSocket
    , WebSocketRequest(..)
    , ReadyState(..)
    , BinaryType(..)
    , connect
    , close
    , send
    , sendArrayBuffer
    , sendBlob
    , getBufferedAmount
    , getExtensions
    , getProtocol
    , getReadyState
    , getBinaryType
    , setBinaryType
    , getUrl
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
    , Enum
    , toEnum
    , fmap
    , userError
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
import           GHCJS.Concurrent
import           GHCJS.Prim
import           GHCJS.Foreign.Callback.Internal (Callback(..))
import qualified GHCJS.Foreign.Callback          as CB

import           GHC.Exts

import           Control.Exception
import           Control.Monad

import           Data.Data
import           Data.Maybe
import           Data.Typeable

import           System.IO

import           Data.JSString (JSString)
import qualified Data.JSString as JSS

import           JavaScript.Array (JSArray)
import qualified JavaScript.Array as JSA
import           JavaScript.TypedArray.ArrayBuffer (ArrayBuffer)
import           JavaScript.Web.Blob (Blob)
import           JavaScript.Web.MessageEvent
import           JavaScript.Web.MessageEvent.Internal
import           JavaScript.Web.CloseEvent
import           JavaScript.Web.CloseEvent.Internal
import           JavaScript.Web.ErrorEvent
import           JavaScript.Web.ErrorEvent.Internal

import Unsafe.Coerce


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~




data WebSocketRequest = WebSocketRequest
  { url       :: JSString
  , protocols :: [JSString]
  , onClose   :: Maybe (CloseEvent -> IO ()) -- ^ called when the connection closes (at most once)
  , onMessage :: Maybe (MessageEvent -> IO ()) -- ^ called for each message
  }

newtype WebSocket = WebSocket JSVal
-- instance IsJSVal WebSocket

data ReadyState = Connecting | Open | Closing | Closed
  deriving (Data, Typeable, Enum, Eq, Ord, Show)

data BinaryType = Blob | ArrayBuffer
  deriving (Data, Typeable, Enum, Eq, Ord, Show)

{- | create a WebSocket -} 
connect :: WebSocketRequest -> IO WebSocket
connect req = do
  mcb <- maybeCallback MessageEvent (onMessage req)
  ccb <- maybeCallback CloseEvent   (onClose req)
  withoutPreemption $ do
    ws <- case protocols req of
           []  -> js_createDefault (url req)
           [x] -> js_createStr     (url req) x
    (js_open ws mcb ccb >>= handleOpenErr >> return ws) `onException` js_close 1000 "Haskell Exception" ws

maybeCallback :: (JSVal -> a) -> Maybe (a -> IO ()) -> IO JSVal
maybeCallback _ Nothing = return jsNull
maybeCallback f (Just g) = do
  Callback cb <- CB.syncCallback1 CB.ContinueAsync (g . f)
  return cb

handleOpenErr :: JSVal -> IO ()
handleOpenErr r
  | isNull r  = return ()
  | otherwise = throwIO (userError "WebSocket failed to connect") -- fixme

{- | close a websocket and release the callbacks -}
close :: Maybe Int -> Maybe JSString -> WebSocket -> IO ()
close value reason ws =
  js_close (fromMaybe 1000 value) (fromMaybe JSS.empty reason) ws
{-# INLINE close #-}

send :: JSString -> WebSocket -> IO ()
send xs ws = js_send xs ws
{-# INLINE send #-}

sendBlob :: Blob -> WebSocket -> IO ()
sendBlob = js_sendBlob
{-# INLINE sendBlob #-}

sendArrayBuffer :: ArrayBuffer -> WebSocket -> IO ()
sendArrayBuffer = js_sendArrayBuffer
{-# INLINE sendArrayBuffer #-}

getBufferedAmount :: WebSocket -> IO Int
getBufferedAmount ws = js_getBufferedAmount ws
{-# INLINE getBufferedAmount #-}

getExtensions :: WebSocket -> IO JSString
getExtensions ws = js_getExtensions ws
{-# INLINE getExtensions #-}

getProtocol :: WebSocket -> IO JSString
getProtocol ws = js_getProtocol ws
{-# INLINE getProtocol #-}

getReadyState :: WebSocket -> IO ReadyState
getReadyState ws = fmap toEnum (js_getReadyState ws)
{-# INLINE getReadyState #-}

getBinaryType :: WebSocket -> IO BinaryType
getBinaryType ws = fmap toEnum (js_getBinaryType ws)
{-# INLINE getBinaryType #-}

getUrl :: WebSocket -> JSString
getUrl ws = js_getUrl ws
{-# INLINE getUrl #-}

getLastError :: WebSocket -> IO (Maybe ErrorEvent)
getLastError ws = do
  le <- js_getLastError ws
  return $ if isNull le then Nothing else Just (ErrorEvent le)
{-# INLINE getLastError #-}

setBinaryType :: BinaryType -> WebSocket -> IO ()
setBinaryType Blob = js_setBinaryType (JSS.pack "blob")
setBinaryType ArrayBuffer = js_setBinaryType (JSS.pack "arraybuffer")

-- -----------------------------------------------------------------------------

js_createDefault :: JSString -> IO WebSocket
js_createDefault = stub

js_createStr :: JSString -> JSString -> IO WebSocket
js_createStr = stub

js_createArr :: JSString -> JSArray -> IO WebSocket
js_createArr = stub

js_open  :: WebSocket -> JSVal -> JSVal -> IO JSVal
js_open = stub

js_close :: Int -> JSString -> WebSocket -> IO ()
js_close = stub

js_send              :: JSString -> WebSocket -> IO ()
js_send = stub

js_sendBlob          :: Blob -> WebSocket -> IO ()
js_sendBlob = stub

js_sendArrayBuffer   :: ArrayBuffer -> WebSocket -> IO ()
js_sendArrayBuffer = stub

js_getBufferedAmount :: WebSocket -> IO Int
js_getBufferedAmount = stub

js_getReadyState     :: WebSocket -> IO Int
js_getReadyState = stub

js_getProtocol       :: WebSocket -> IO JSString
js_getProtocol = stub

js_getExtensions     :: WebSocket -> IO JSString
js_getExtensions = stub

js_getUrl            :: WebSocket -> JSString
js_getUrl = stub

js_getBinaryType                             :: WebSocket -> IO Int
js_getBinaryType = stub

js_getLastError      :: WebSocket -> IO JSVal
js_getLastError = stub

js_setBinaryType                             :: JSString -> WebSocket -> IO ()
js_setBinaryType = stub





