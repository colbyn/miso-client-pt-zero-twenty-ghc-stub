{-# LANGUAGE NoImplicitPrelude #-}
module JavaScript.Array
    ( JSArray
    , MutableJSArray
    , create
    , length
    , lengthIO
    , null
    , fromList
    , fromListIO
    , toList
    , toListIO
    , index, (!)
    , read
    , write
    , append
    , push
    , pop
    , unshift
    , shift
    , reverse
    , take
    , takeIO
    , drop
    , dropIO
    , slice
    , sliceIO
    , freeze
    , unsafeFreeze
    , thaw
    , unsafeThaw
    )
where


-- ~
import Core hiding (null)
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
import           Prelude hiding (length, drop, read, take, reverse, null)

import qualified GHCJS.Prim    as Prim
import           GHCJS.Types

import           JavaScript.Array.Internal (JSArray(..))
import           JavaScript.Array.Internal



-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~



-- import qualified JavaScript.Array.Internal as I
{-
fromList :: [JSVal] -> IO (JSArray a)
fromList xs = fmap JSArray (I.fromList xs)
{-# INLINE fromList #-}

toList :: JSArray a -> IO [JSVal]
toList (JSArray x) = I.toList x
{-# INLINE toList #-}

create :: IO (JSArray a)
create = fmap JSArray I.create
{-# INLINE create #-}

length :: JSArray a -> IO Int
length (JSArray x) = I.length x
{-# INLINE length #-}

append :: JSArray a -> JSArray a -> IO (JSArray a)
append (JSArray x) (JSArray y) = fmap JSArray (I.append x y)
{-# INLINE append #-}
-}

(!) :: JSArray -> Int -> JSVal
x ! n = index n x
{-# INLINE (!) #-}

{-

index :: Int -> JSArray a -> IO JSVal
index n (JSArray x) = I.index n x
{-# INLINE index #-}

write :: Int -> JSVal -> JSArray a -> IO ()
write n e (JSArray x) = I.write n e x
{-# INLINE write #-}

drop :: Int -> JSArray a -> IO (JSArray a)
drop n (JSArray x) = fmap JSArray (I.drop n x)
{-# INLINE drop #-}

take :: Int -> JSArray a -> IO (JSArray a)
take n (JSArray x) = fmap JSArray (I.take n x)
{-# INLINE take #-}

slice :: Int -> Int -> JSArray a -> IO (JSArray a)
slice s n (JSArray x) = fmap JSArray (I.slice s n x)
{-# INLINE slice #-}

push :: JSVal -> JSArray a -> IO ()
push e (JSArray x) = I.push e x
{-# INLINE push #-}

pop :: JSArray a -> IO JSVal
pop (JSArray x) = I.pop x
{-# INLINE pop #-}

unshift :: JSVal -> JSArray a -> IO ()
unshift e (JSArray x) = I.unshift e x
{-# INLINE unshift #-}

shift :: JSArray a -> IO JSVal
shift (JSArray x) = I.shift x
{-# INLINE shift #-}

reverse :: JSArray a -> IO ()
reverse (JSArray x) = I.reverse x
{-# INLINE reverse #-}
-}





