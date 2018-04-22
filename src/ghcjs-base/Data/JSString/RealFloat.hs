{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, MagicHash,
             UnliftedFFITypes
  #-}
module Data.JSString.RealFloat
    ( FPFormat(..)
    , realFloat
    , formatRealFloat
    , formatDouble
    , formatFloat
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
    , RealFloat
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
import GHC.Exts (Int#, Float#, Double#, Int(..), Float(..), Double(..))

import Data.JSString


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


-- | Control the rendering of floating point numbers.
data FPFormat = Exponent
              -- ^ Scientific notation (e.g. @2.3e123@).
              | Fixed
              -- ^ Standard decimal notation.
              | Generic
              -- ^ Use decimal notation for values between @0.1@ and
              -- @9,999,999@, and scientific notation otherwise.
                deriving (Enum, Read, Show)

realFloat :: (RealFloat a) => a -> JSString
realFloat = error "Data.JSString.RealFloat.realFloat not yet implemented"
{-# RULES "realFloat/Double" realFloat = genericDouble #-}
{-# RULES "realFoat/Float"   realFloat = genericFloat  #-}
{-# NOINLINE realFloat #-}

formatRealFloat :: (RealFloat a)
                => FPFormat
                -> Maybe Int
                -> a
                -> JSString
formatRealFloat = error "Data.JSString.RealFloat.formatRealFloat not yet implemented"
{-# RULES "formatRealFloat/Double" formatRealFloat = formatDouble #-}
{-# RULES "formatRealFloat/Float"  formatRealFloat = formatFloat  #-}
{-# NOINLINE formatRealFloat #-}

genericDouble :: Double -> JSString
genericDouble (D# d) = js_doubleGeneric -1# d
{-# INLINE genericDouble #-}

genericFloat :: Float -> JSString
genericFloat (F# f) = js_floatGeneric -1# f
{-# INLINE genericFloat #-}

formatDouble :: FPFormat -> Maybe Int -> Double -> JSString
formatDouble fmt Nothing (D# d)
  = case fmt of
     Fixed    -> js_doubleToFixed -1# d
     Exponent -> js_doubleToExponent -1# d
     Generic  -> js_doubleGeneric -1# d
formatDouble fmt (Just (I# decs)) (D# d)
  = case fmt of
      Fixed    -> js_doubleToFixed decs d
      Exponent -> js_doubleToExponent decs d
      Generic  -> js_doubleGeneric decs d
{-# INLINE formatDouble #-}

formatFloat :: FPFormat -> Maybe Int -> Float -> JSString
formatFloat fmt Nothing (F# f)
  = case fmt of
     Fixed    -> js_floatToFixed -1# f
     Exponent -> js_floatToExponent -1# f
     Generic  -> js_floatGeneric -1# f
formatFloat fmt (Just (I# decs)) (F# f)
  = case fmt of
      Fixed    -> js_floatToFixed decs f
      Exponent -> js_floatToExponent decs f
      Generic  -> js_floatGeneric decs f
{-# INLINE formatFloat #-}


js_doubleToFixed :: Int# -> Double# -> JSString
js_doubleToFixed = stub

js_floatToFixed :: Int# -> Float# -> JSString
js_floatToFixed = stub

js_doubleToExponent :: Int# -> Double# -> JSString
js_doubleToExponent = stub

js_floatToExponent :: Int# -> Float# -> JSString
js_floatToExponent = stub

js_doubleGeneric :: Int# -> Double# -> JSString
js_doubleGeneric = stub

js_floatGeneric :: Int# -> Float# -> JSString
js_floatGeneric = stub

                        
