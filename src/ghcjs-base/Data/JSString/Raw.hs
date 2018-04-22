{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI,
             MagicHash, UnboxedTuples, UnliftedFFITypes, GHCForeignImportPrim
  #-}

{-
  Low level bindings for JavaScript strings. These expose the underlying
  encoding. Use Data.JSString for 
 -}
module Data.JSString.Raw
    ( rawHead
    , rawTail
    , rawInit
    , rawLast
    , rawLength
    , rawTake
    , rawDrop
    , rawTakeEnd
    , rawDropEnd
    , rawChunksOf
    , rawChunksOf'
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
import           GHC.Exts
  ( Int(..), Int#, Char(..)
  , negateInt#
  , (+#), (-#), (>=#), (<#)
  , isTrue#, chr#)
import qualified GHC.Exts as Exts

import Unsafe.Coerce

import Data.JSString.Internal.Type


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


rawLength :: JSString -> Int
rawLength x = I# (js_length x)
{-# INLINE rawLength #-}

rawHead :: JSString -> Char
rawHead x
  | js_null x = emptyError "rawHead"
  | otherwise = C# (chr# (js_charCodeAt 0# x))
{-# INLINE rawHead #-}

unsafeRawHead :: JSString -> Char
unsafeRawHead x = C# (chr# (js_charCodeAt 0# x))
{-# INLINE unsafeRawHead #-}

rawLast :: JSString -> Char
rawLast x
  | js_null x = emptyError "rawLast"
  | otherwise = C# (chr# (js_charCodeAt (js_length x -# 1#) x))
{-# INLINE rawLast #-}

unsafeRawLast :: JSString -> Char
unsafeRawLast x = C# (chr# (js_charCodeAt (js_length x -# 1#) x))
{-# INLINE unsafeRawLast #-}

rawTail :: JSString -> JSString
rawTail x
  | js_null x = emptyError "rawTail"
  | otherwise = js_substr1 1# x
{-# INLINE rawTail #-}

unsafeRawTail :: JSString -> JSString
unsafeRawTail x = js_substr1 1# x
{-# INLINE unsafeRawTail #-}

rawInit :: JSString -> JSString
rawInit x = js_substr 0# (js_length x -# 1#) x
{-# INLINE rawInit #-}

unsafeRawInit :: JSString -> JSString
unsafeRawInit x = js_substr 0# (js_length x -# 1#) x
{-# INLINE unsafeRawInit #-}

unsafeRawIndex :: Int -> JSString -> Char
unsafeRawIndex (I# n) x = C# (chr# (js_charCodeAt n x))
{-# INLINE unsafeRawIndex #-}

rawIndex :: Int -> JSString -> Char
rawIndex (I# n) x
  | isTrue# (n <# 0#) || isTrue# (n >=# js_length x) =
      overflowError "rawIndex"
  | otherwise = C# (chr# (js_charCodeAt n x))
{-# INLINE rawIndex #-}
    
rawTake :: Int -> JSString -> JSString
rawTake (I# n) x = js_substr 0# n x
{-# INLINE rawTake #-}

rawDrop :: Int -> JSString -> JSString
rawDrop (I# n) x = js_substr1 n x
{-# INLINE rawDrop #-}

rawTakeEnd :: Int -> JSString -> JSString
rawTakeEnd (I# k) x = js_slice1 (negateInt# k) x
{-# INLINE rawTakeEnd #-}

rawDropEnd :: Int -> JSString -> JSString
rawDropEnd (I# k) x = js_substr 0# (js_length x -# k) x
{-# INLINE rawDropEnd #-}

rawChunksOf :: Int -> JSString -> [JSString]
rawChunksOf (I# k) x =
  let l     = js_length x
      go i = case i >=# l of
               0# -> js_substr i k x : go (i +# k)
               _  -> []
  in go 0#
{-# INLINE rawChunksOf #-}

rawChunksOf' :: Int -> JSString -> [JSString]
rawChunksOf' (I# k) x = unsafeCoerce (js_rawChunksOf k x)
{-# INLINE rawChunksOf' #-}

rawSplitAt :: Int -> JSString -> (JSString, JSString)
rawSplitAt (I# k) x = (js_substr 0# k x, js_substr1 k x)
{-# INLINE rawSplitAt #-}

emptyError :: String -> a
emptyError fun = error $ "Data.JSString.Raw." ++ fun ++ ": empty input"

overflowError :: String -> a
overflowError fun = error $ "Data.JSString.Raw." ++ fun ++ ": size overflow"

-- -----------------------------------------------------------------------------

js_null :: JSString -> Bool
js_null = stub

js_length :: JSString -> Int#
js_length = stub

js_substr :: Int# -> Int# -> JSString -> JSString
js_substr = stub

js_substr1 :: Int# -> JSString -> JSString
js_substr1 = stub

js_slice :: Int# -> Int# -> JSString -> JSString
js_slice = stub

js_slice1 :: Int# -> JSString -> JSString
js_slice1 = stub

js_indexOf :: JSString -> Int# -> JSString -> Int#
js_indexOf = stub

js_indexOf1 :: JSString -> JSString -> Int#
js_indexOf1 = stub

js_charCodeAt :: Int# -> JSString -> Int#
js_charCodeAt = stub

js_rawChunksOf :: Int# -> JSString -> Exts.Any -- [JSString]
js_rawChunksOf = stub





