{-# LANGUAGE NoImplicitPrelude #-}
module JavaScript.Web.ErrorEvent
    ( ErrorEvent
    , message
    , filename
    , lineno
    , colno
    , error
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

import JavaScript.Web.ErrorEvent.Internal


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


message :: ErrorEvent -> JSString
message ee = js_getMessage ee
{-# INLINE message #-}

filename :: ErrorEvent -> JSString
filename ee = js_getFilename ee
{-# INLINE filename #-}

lineno :: ErrorEvent -> Int
lineno ee = js_getLineno ee
{-# INLINE lineno #-}

colno :: ErrorEvent -> Int
colno ee = js_getColno ee
{-# INLINE colno #-}

error :: ErrorEvent -> JSVal
error ee = js_getError ee
{-# INLINE error #-}

-- -----------------------------------------------------------------------------

js_getMessage  :: ErrorEvent -> JSString
js_getMessage = stub

js_getFilename :: ErrorEvent -> JSString
js_getFilename = stub

js_getLineno   :: ErrorEvent -> Int
js_getLineno = stub

js_getColno    :: ErrorEvent -> Int
js_getColno = stub

js_getError    :: ErrorEvent -> JSVal
js_getError = stub

