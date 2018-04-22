{-# LANGUAGE NoImplicitPrelude #-}
module JavaScript.Web.Location
    ( Location
    , getWindowLocation
    , getHref
    , setHref
    , getProtocol
    , setProtocol
    , getHost
    , setHost
    , getHostname
    , setHostname
    , getPort
    , setPort
    , getPathname
    , setPathname
    , getSearch
    , setSearch
    , getHash
    , setHash
    , getUsername
    , setUsername
    , getPassword
    , setPassword
    , getOrigin
    , assign
    , reload
    , replace
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
-- Web/GHCJS Specific
-- ---------------
import           Data.Typeable

import           Data.JSString (JSString)
import qualified Data.JSString as JSS

import           GHCJS.Types


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


newtype Location = Location JSVal deriving (Typeable)
instance IsJSVal Location

getWindowLocation :: IO Location
getWindowLocation = js_getWindowLocation
{-# INLINE getWindowLocation #-}

getHref :: Location -> IO JSString
getHref = js_getHref
{-# INLINE getHref #-}

setHref :: JSString -> Location -> IO ()
setHref = js_setHref
{-# INLINE setHref #-}

getProtocol :: Location -> IO JSString
getProtocol = js_getProtocol
{-# INLINE getProtocol #-}

setProtocol :: JSString -> Location -> IO ()
setProtocol = js_setProtocol
{-# INLINE setProtocol #-}

getHost :: Location -> IO JSString
getHost = js_getHost
{-# INLINE getHost #-}

setHost :: JSString -> Location -> IO ()
setHost = js_setHost
{-# INLINE setHost #-}

getHostname :: Location -> IO JSString
getHostname = js_getHostname
{-# INLINE getHostname #-}

setHostname :: JSString -> Location -> IO ()
setHostname = js_setHostname
{-# INLINE setHostname #-}

getPort :: Location -> IO JSString
getPort = js_getPort
{-# INLINE getPort #-}

setPort :: JSString -> Location -> IO ()
setPort = js_setPort
{-# INLINE setPort #-}

getPathname :: Location -> IO JSString
getPathname = js_getPathname
{-# INLINE getPathname #-}

setPathname :: JSString -> Location -> IO ()
setPathname = js_setPathname
{-# INLINE setPathname #-}

getSearch :: Location -> IO JSString
getSearch = js_getSearch
{-# INLINE getSearch #-}

setSearch :: JSString -> Location -> IO ()
setSearch = js_setSearch
{-# INLINE setSearch #-}

getHash :: Location -> IO JSString
getHash = js_getHash
{-# INLINE getHash #-}

setHash :: JSString -> Location -> IO ()
setHash = js_setHash
{-# INLINE setHash #-}

getUsername :: Location -> IO JSString
getUsername = js_getUsername
{-# INLINE getUsername #-}

setUsername :: JSString -> Location -> IO ()
setUsername = js_setUsername
{-# INLINE setUsername #-}

getPassword :: Location -> IO JSString
getPassword = js_getPassword
{-# INLINE getPassword #-}

setPassword :: JSString -> Location -> IO ()
setPassword = js_setPassword
{-# INLINE setPassword #-}

getOrigin :: Location -> IO JSString
getOrigin = js_getUsername
{-# INLINE getOrigin #-}

assign :: JSString -> Location -> IO ()
assign = js_assign
{-# INLINE assign #-}

reload :: Bool -> Location -> IO ()
reload = js_reload
{-# INLINE reload #-}

replace :: JSString -> Location -> IO ()
replace = js_assign
{-# INLINE replace #-}

-------------------------------------------------------------------------------

js_getWindowLocation :: IO Location
js_getWindowLocation = stub

js_getHref     :: Location -> IO JSString
js_getHref = stub

js_getProtocol :: Location -> IO JSString
js_getProtocol = stub

js_getHost     :: Location -> IO JSString
js_getHost = stub

js_getHostname :: Location -> IO JSString
js_getHostname = stub

js_getPort     :: Location -> IO JSString
js_getPort = stub

js_getPathname :: Location -> IO JSString
js_getPathname = stub

js_getSearch   :: Location -> IO JSString
js_getSearch = stub

js_getHash     :: Location -> IO JSString
js_getHash = stub

js_getUsername :: Location -> IO JSString
js_getUsername = stub

js_getPassword :: Location -> IO JSString
js_getPassword = stub

js_getOrigin   :: Location -> IO JSString
js_getOrigin = stub


js_setHref     :: JSString -> Location -> IO ()
js_setHref = stub

js_setProtocol :: JSString -> Location -> IO ()
js_setProtocol = stub

js_setHost     :: JSString -> Location -> IO ()
js_setHost = stub

js_setHostname :: JSString -> Location -> IO ()
js_setHostname = stub

js_setPort     :: JSString -> Location -> IO ()
js_setPort = stub

js_setPathname :: JSString -> Location -> IO ()
js_setPathname = stub

js_setSearch   :: JSString -> Location -> IO ()
js_setSearch = stub

js_setHash     :: JSString -> Location -> IO ()
js_setHash = stub

js_setUsername :: JSString -> Location -> IO ()
js_setUsername = stub

js_setPassword :: JSString -> Location -> IO ()
js_setPassword = stub


js_assign      :: JSString -> Location -> IO ()
js_assign = stub

js_reload      :: Bool     -> Location -> IO ()
js_reload = stub

js_replace     :: JSString -> Location -> IO ()
js_replace = stub




