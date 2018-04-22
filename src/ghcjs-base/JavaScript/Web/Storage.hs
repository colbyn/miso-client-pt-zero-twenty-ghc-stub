{-# LANGUAGE NoImplicitPrelude #-}
module JavaScript.Web.Storage
    ( localStorage
    , sessionStorage
    , Storage
    , getLength
    , getIndex
    , getItem
    , setItem
    , removeItem
    , clear
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
import GHCJS.Types

import Data.JSString
import Data.JSString.Internal.Type

import JavaScript.Web.Storage.Internal


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~



localStorage :: Storage
localStorage = js_localStorage
{-# INLINE localStorage #-}

sessionStorage :: Storage
sessionStorage = js_sessionStorage
{-# INLINE sessionStorage #-}

getLength :: Storage -> IO Int
getLength s = js_getLength s
{-# INLINE getLength #-}

getIndex :: Int -> Storage -> IO (Maybe JSString)
getIndex i s = do
  r <- js_getIndex i s
  return $ if isNull r then Nothing else Just (JSString r)
{-# INLINE getIndex #-}

getItem :: JSString -> Storage -> IO (Maybe JSString)
getItem key s = do
  r <- js_getItem key s
  return $ if isNull r then Nothing else Just (JSString r)
{-# INLINE getItem #-}

setItem :: JSString -> JSString -> Storage -> IO ()
setItem key val s = js_setItem key val s
{-# INLINE setItem #-}

removeItem :: JSString -> Storage -> IO ()
removeItem key s = js_removeItem key s
{-# INLINE removeItem #-}

clear :: Storage -> IO ()
clear s = js_clear s
{-# INLINE clear #-}

-- -----------------------------------------------------------------------------

js_localStorage   :: Storage
js_localStorage = stub

js_sessionStorage :: Storage
js_sessionStorage = stub

js_getLength      :: Storage -> IO Int
js_getLength = stub

js_getIndex       :: Int -> Storage -> IO JSVal
js_getIndex = stub

js_getItem        :: JSString -> Storage -> IO JSVal
js_getItem = stub

js_setItem        :: JSString -> JSString -> Storage -> IO ()
js_setItem = stub

js_removeItem     :: JSString -> Storage -> IO ()
js_removeItem = stub

js_clear          :: Storage -> IO ()
js_clear = stub




