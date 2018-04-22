{-# LANGUAGE NoImplicitPrelude #-}
module JavaScript.Web.Canvas.TextMetrics
    ( width
    , actualBoundingBoxLeft
    , actualBoundingBoxRight
    , fontBoundingBoxAscent
    , fontBoundingBoxDescent
    , actualBoundingBoxAscent
    , actualBoundingBoxDescent
    , emHeightAscent
    , emHeightDescent
    , hangingBaseline
    , alphabeticBaseline
    , ideographicBaseline
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
import JavaScript.Web.Canvas.Internal


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~



width :: TextMetrics -> Double
width tm = js_width tm
{-# INLINE width #-}

actualBoundingBoxLeft :: TextMetrics -> Double
actualBoundingBoxLeft tm = js_actualBoundingBoxLeft tm
{-# INLINE actualBoundingBoxLeft #-}

actualBoundingBoxRight :: TextMetrics -> Double
actualBoundingBoxRight tm = js_actualBoundingBoxRight tm
{-# INLINE actualBoundingBoxRight #-}

fontBoundingBoxAscent :: TextMetrics -> Double
fontBoundingBoxAscent tm = js_fontBoundingBoxAscent tm
{-# INLINE fontBoundingBoxAscent #-}

fontBoundingBoxDescent :: TextMetrics -> Double
fontBoundingBoxDescent tm = js_fontBoundingBoxDescent tm
{-# INLINE fontBoundingBoxDescent #-}

actualBoundingBoxAscent :: TextMetrics -> Double
actualBoundingBoxAscent tm = js_actualBoundingBoxAscent tm
{-# INLINE actualBoundingBoxAscent #-}

actualBoundingBoxDescent :: TextMetrics -> Double
actualBoundingBoxDescent tm = js_actualBoundingBoxDescent tm
{-# INLINE actualBoundingBoxDescent #-}

emHeightAscent :: TextMetrics -> Double
emHeightAscent tm = js_emHeightAscent tm
{-# INLINE emHeightAscent #-}

emHeightDescent :: TextMetrics -> Double
emHeightDescent tm = js_emHeightDescent tm
{-# INLINE emHeightDescent #-}

hangingBaseline :: TextMetrics -> Double
hangingBaseline tm = js_hangingBaseline tm
{-# INLINE hangingBaseline #-}

alphabeticBaseline :: TextMetrics -> Double
alphabeticBaseline tm = js_alphabeticBaseline tm
{-# INLINE alphabeticBaseline #-}

ideographicBaseline :: TextMetrics -> Double
ideographicBaseline tm = js_ideographicBaseline tm
{-# INLINE ideographicBaseline #-}

-- -----------------------------------------------------------------------------

js_width :: TextMetrics -> Double
js_width = stub

js_actualBoundingBoxLeft :: TextMetrics -> Double
js_actualBoundingBoxLeft = stub

js_actualBoundingBoxRight :: TextMetrics -> Double
js_actualBoundingBoxRight = stub

js_fontBoundingBoxAscent :: TextMetrics -> Double
js_fontBoundingBoxAscent = stub

js_fontBoundingBoxDescent :: TextMetrics -> Double
js_fontBoundingBoxDescent = stub

js_actualBoundingBoxAscent :: TextMetrics -> Double
js_actualBoundingBoxAscent = stub

js_actualBoundingBoxDescent :: TextMetrics -> Double
js_actualBoundingBoxDescent = stub

js_emHeightAscent :: TextMetrics -> Double
js_emHeightAscent = stub

js_emHeightDescent :: TextMetrics -> Double
js_emHeightDescent = stub

js_hangingBaseline :: TextMetrics -> Double
js_hangingBaseline = stub

js_alphabeticBaseline :: TextMetrics -> Double
js_alphabeticBaseline = stub

js_ideographicBaseline :: TextMetrics -> Double
js_ideographicBaseline = stub








