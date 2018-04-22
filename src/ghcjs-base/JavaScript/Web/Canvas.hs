{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE EmptyDataDecls,  OverloadedStrings, DeriveDataTypeable #-}
module JavaScript.Web.Canvas
    ( Context
    , Canvas
    , Image
    , TextAlign(..)
    , TextBaseline(..)
    , LineCap(..)
    , LineJoin(..)
    , Repeat(..)
    , Gradient
    , Pattern
    , create
    , unsafeToCanvas
    , toCanvas
    , getContext
    , save
    , restore
    , scale
    , rotate
    , translate
    , transform
    , setTransform
    , fill
    , fillRule
    , stroke
    , beginPath
    , closePath
    , clip
    , moveTo
    , lineTo
    , quadraticCurveTo
    , bezierCurveTo
    , arc
    , arcTo
    , rect
    , isPointInPath
    , fillStyle
    , strokeStyle
    , globalAlpha
    , lineJoin
    , lineCap
    , lineWidth
    , setLineDash
    , lineDashOffset
    , miterLimit
    , fillText
    , strokeText
    , font
    , measureText
    , textAlign
    , textBaseline
    , fillRect
    , strokeRect
    , clearRect
    , drawImage
    , width
    , setWidth
    , height
    , setHeight
    )
where


-- ~
import Core hiding (Left, Right)
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
import Control.Applicative
import Control.Monad

import Data.Data
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Typeable

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import           JavaScript.Web.Canvas.Internal

import           JavaScript.Object (Object)
import qualified JavaScript.Object as O
import           JavaScript.Array  (JSArray)
import qualified JavaScript.Array  as A


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


data TextAlign = Start
               | End
               | Left
               | Right
               | Center
             deriving (Eq, Show, Enum, Data, Typeable)

data TextBaseline = Top 
                  | Hanging 
                  | Middle
                  | Alphabetic
                  | Ideographic
                  | Bottom
                deriving (Eq, Show, Enum, Data, Typeable)

data LineJoin = LineJoinBevel
              | LineJoinRound
              | LineJoinMiter
            deriving (Eq, Show, Enum)

data LineCap = LineCapButt
             | LineCapRound
             | LineCapSquare deriving (Eq, Show, Enum, Data, Typeable)

data Repeat = Repeat
            | RepeatX
            | RepeatY
            | NoRepeat
            deriving (Eq, Ord, Show, Enum, Data, Typeable)

unsafeToCanvas :: JSVal -> Canvas
unsafeToCanvas r = Canvas r
{-# INLINE unsafeToCanvas #-}

toCanvas :: JSVal -> Maybe Canvas
toCanvas x = error "toCanvas" -- fixme
{-# INLINE toCanvas #-}

create :: Int -> Int -> IO Canvas
create = js_create
{-# INLINE create #-}

getContext :: Canvas -> IO Context
getContext c = js_getContext c
{-# INLINE getContext #-}

save :: Context -> IO ()
save ctx = js_save ctx
{-# INLINE save #-}

restore :: Context -> IO ()
restore = js_restore
{-# INLINE restore #-}

transform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
transform = js_transform
{-# INLINE transform #-}

setTransform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
setTransform = js_setTransform
{-# INLINE setTransform #-}

scale :: Double -> Double -> Context -> IO ()
scale x y ctx = js_scale x y ctx
{-# INLINE scale #-}

translate :: Double -> Double -> Context -> IO ()
translate x y ctx = js_translate x y ctx
{-# INLINE translate #-}

rotate :: Double -> Context -> IO ()
rotate r ctx = js_rotate r ctx
{-# INLINE rotate #-}

fill :: Context -> IO ()
fill ctx = js_fill ctx
{-# INLINE fill #-}

fillRule :: JSString -> Context -> IO ()
fillRule rule ctx = js_fill_rule rule ctx
{-# INLINE fillRule #-}

stroke :: Context -> IO ()
stroke = js_stroke
{-# INLINE stroke #-}

beginPath :: Context -> IO ()
beginPath = js_beginPath
{-# INLINE beginPath #-}

closePath :: Context -> IO ()
closePath = js_closePath
{-# INLINE closePath #-}

clip :: Context -> IO ()
clip = js_clip
{-# INLINE clip #-}

moveTo :: Double -> Double -> Context -> IO ()
moveTo = js_moveTo
{-# INLINE moveTo #-}

lineTo :: Double -> Double -> Context -> IO ()
lineTo = js_lineTo
{-# INLINE lineTo #-}

quadraticCurveTo :: Double -> Double -> Double -> Double -> Context -> IO ()
quadraticCurveTo = js_quadraticCurveTo
{-# INLINE quadraticCurveTo #-}

bezierCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
bezierCurveTo = js_bezierCurveTo
{-# INLINE bezierCurveTo #-}

arc :: Double -> Double -> Double -> Double -> Double -> Bool -> Context -> IO ()
arc a b c d e bl ctx = js_arc a b c d e bl ctx
{-# INLINE arc #-}

arcTo :: Double -> Double -> Double -> Double -> Double -> Context -> IO ()
arcTo = js_arcTo
{-# INLINE arcTo #-}

rect :: Double -> Double -> Double -> Double -> Context -> IO ()
rect = js_rect
{-# INLINE rect #-}

isPointInPath :: Double -> Double -> Context -> IO ()
isPointInPath = js_isPointInPath
{-# INLINE isPointInPath #-}

fillStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
fillStyle = js_fillStyle
{-# INLINE fillStyle #-}

strokeStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
strokeStyle = js_strokeStyle
{-# INLINE strokeStyle #-}

globalAlpha :: Double -> Context -> IO ()
globalAlpha = js_globalAlpha
{-# INLINE globalAlpha #-}

lineJoin :: LineJoin -> Context -> IO ()
lineJoin LineJoinBevel ctx = js_lineJoin "bevel" ctx
lineJoin LineJoinRound ctx = js_lineJoin "round" ctx
lineJoin LineJoinMiter ctx = js_lineJoin "miter" ctx
{-# INLINE lineJoin #-}

lineCap :: LineCap -> Context -> IO ()
lineCap LineCapButt   ctx = js_lineCap "butt"   ctx
lineCap LineCapRound  ctx = js_lineCap "round"  ctx
lineCap LineCapSquare ctx = js_lineCap "square" ctx
{-# INLINE lineCap #-}

miterLimit :: Double -> Context -> IO ()
miterLimit = js_miterLimit
{-# INLINE miterLimit #-}

-- | pass an array of numbers
setLineDash :: JSArray -> Context -> IO ()
setLineDash arr ctx = js_setLineDash arr ctx
{-# INLINE setLineDash #-}

lineDashOffset :: Double -> Context -> IO ()
lineDashOffset = js_lineDashOffset
{-# INLINE lineDashOffset #-}

textAlign :: TextAlign -> Context -> IO ()
textAlign align ctx = case align of
     Start  -> js_textAlign "start"  ctx
     End    -> js_textAlign "end"    ctx
     Left   -> js_textAlign "left"   ctx
     Right  -> js_textAlign "right"  ctx
     Center -> js_textAlign "center" ctx
{-# INLINE textAlign #-}

textBaseline :: TextBaseline -> Context -> IO ()
textBaseline baseline ctx = case baseline of 
     Top         -> js_textBaseline "top"         ctx
     Hanging     -> js_textBaseline "hanging"     ctx
     Middle      -> js_textBaseline "middle"      ctx
     Alphabetic  -> js_textBaseline "alphabetic"  ctx
     Ideographic -> js_textBaseline "ideographic" ctx
     Bottom      -> js_textBaseline "bottom"      ctx
{-# INLINE textBaseline #-}

lineWidth :: Double -> Context -> IO ()
lineWidth = js_lineWidth
{-# INLINE lineWidth #-}

fillText :: JSString -> Double -> Double -> Context -> IO ()
fillText t x y ctx = js_fillText t x y ctx
{-# INLINE fillText #-}

strokeText :: JSString -> Double -> Double -> Context -> IO ()
strokeText t x y ctx = js_strokeText t x y ctx
{-# INLINE strokeText #-}

font :: JSString -> Context -> IO ()
font f ctx = js_font f ctx
{-# INLINE font #-}

measureText :: JSString -> Context -> IO Double
measureText t ctx = stub
{-# INLINE measureText #-}

fillRect :: Double -> Double -> Double -> Double -> Context -> IO ()
fillRect = js_fillRect
{-# INLINE fillRect #-}

clearRect :: Double -> Double -> Double -> Double -> Context -> IO ()
clearRect = js_clearRect
{-# INLINE clearRect #-}

strokeRect :: Double -> Double -> Double -> Double -> Context -> IO ()
strokeRect = js_strokeRect
{-# INLINE strokeRect #-}

drawImage :: Image -> Int -> Int -> Int -> Int -> Context -> IO ()
drawImage = js_drawImage
{-# INLINE drawImage #-}

createPattern :: Image -> Repeat -> Context -> IO Pattern
createPattern img Repeat   ctx = js_createPattern img "repeat"    ctx
createPattern img RepeatX  ctx = js_createPattern img "repeat-x"  ctx
createPattern img RepeatY  ctx = js_createPattern img "repeat-y"  ctx
createPattern img NoRepeat ctx = js_createPattern img "no-repeat" ctx
{-# INLINE createPattern #-}

setWidth :: Int -> Canvas -> IO ()
setWidth w c = js_setWidth w c
{-# INLINE setWidth #-}

width :: Canvas -> IO Int
width c = js_width c
{-# INLINE width #-}

setHeight :: Int -> Canvas -> IO ()
setHeight h c = js_setHeight h c
{-# INLINE setHeight #-}

height :: Canvas -> IO Int
height c = js_height c
{-# INLINE height #-}

-- ----------------------------------------------------------------------------


js_create :: Int -> Int -> IO Canvas
js_create = stub

js_getContext :: Canvas -> IO Context
js_getContext = stub

js_save :: Context -> IO ()
js_save = stub

js_restore  :: Context -> IO ()
js_restore = stub

js_transform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
js_transform = stub

js_setTransform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
js_setTransform = stub

js_scale :: Double -> Double -> Context -> IO ()
js_scale = stub

js_translate  :: Double -> Double -> Context -> IO ()
js_translate = stub

js_rotate :: Double -> Context -> IO ()
js_rotate = stub

js_fill :: Context -> IO ()
js_fill = stub

js_fill_rule  :: JSString -> Context -> IO ()
js_fill_rule = stub

js_stroke :: Context -> IO ()
js_stroke = stub

js_beginPath :: Context -> IO ()
js_beginPath = stub

js_closePath :: Context -> IO ()
js_closePath = stub

js_clip  :: Context -> IO ()
js_clip = stub

js_moveTo :: Double -> Double -> Context -> IO ()
js_moveTo = stub

js_lineTo :: Double -> Double -> Context -> IO ()
js_lineTo = stub

js_quadraticCurveTo :: Double -> Double -> Double -> Double -> Context -> IO ()
js_quadraticCurveTo = stub

js_bezierCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
js_bezierCurveTo = stub

js_arc :: Double -> Double -> Double -> Double -> Double -> Bool -> Context -> IO ()
js_arc = stub

js_arcTo :: Double -> Double -> Double -> Double -> Double -> Context -> IO ()
js_arcTo = stub

js_rect :: Double -> Double -> Double -> Double -> Context -> IO ()
js_rect = stub

js_isPointInPath :: Double -> Double -> Context -> IO ()
js_isPointInPath = stub

js_fillStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
js_fillStyle = stub

js_strokeStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
js_strokeStyle = stub

js_globalAlpha :: Double           -> Context -> IO ()
js_globalAlpha = stub

js_lineJoin :: JSString -> Context -> IO ()
js_lineJoin = stub

js_lineCap :: JSString -> Context -> IO ()
js_lineCap = stub

js_miterLimit :: Double -> Context -> IO ()
js_miterLimit = stub

js_setLineDash :: JSArray -> Context -> IO ()
js_setLineDash = stub

js_lineDashOffset :: Double -> Context -> IO ()
js_lineDashOffset = stub

js_font :: JSString -> Context -> IO ()
js_font = stub

js_textAlign :: JSString -> Context -> IO ()
js_textAlign = stub

js_textBaseline :: JSString -> Context -> IO ()
js_textBaseline = stub

js_lineWidth :: Double -> Context -> IO ()
js_lineWidth = stub

js_fillText :: JSString -> Double -> Double -> Context -> IO ()
js_fillText = stub

js_strokeText :: JSString -> Double -> Double -> Context -> IO ()
js_strokeText = stub

js_measureText :: JSString                    -> Context -> IO Object
js_measureText = stub

js_fillRect :: Double -> Double -> Double -> Double -> Context -> IO ()
js_fillRect = stub

js_clearRect :: Double -> Double -> Double -> Double -> Context -> IO ()
js_clearRect = stub

js_strokeRect :: Double -> Double -> Double -> Double -> Context -> IO ()
js_strokeRect = stub

js_drawImage :: Image -> Int -> Int -> Int -> Int -> Context -> IO () 
js_drawImage = stub

js_createPattern :: Image -> JSString -> Context -> IO Pattern
js_createPattern = stub

js_width :: Canvas -> IO Int
js_width = stub

js_height :: Canvas -> IO Int
js_height = stub

js_setWidth :: Int -> Canvas -> IO ()
js_setWidth = stub

js_setHeight :: Int -> Canvas -> IO ()
js_setHeight = stub





