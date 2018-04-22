{-# LANGUAGE NoImplicitPrelude #-}


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}

module JavaScript.Web.Blob.Internal where


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
import Data.Typeable

import GHCJS.Types


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


data BlobType = BlobTypeBlob
              | BlobTypeFile

newtype SomeBlob (a :: BlobType) = SomeBlob JSVal deriving Typeable

type File = SomeBlob BlobTypeFile
type Blob = SomeBlob BlobTypeBlob
  
size :: SomeBlob a -> Int
size b = js_size b
{-# INLINE size #-}

contentType :: SomeBlob a -> JSString
contentType b = js_type b
{-# INLINE contentType #-}

-- is the type correct, does slicing a File give another File?
slice :: Int -> Int -> JSString -> SomeBlob a -> SomeBlob a
slice start end contentType b = js_slice start end contentType b
{-# INLINE slice #-}

isClosed :: SomeBlob a -> IO Bool
isClosed b = js_isClosed b
{-# INLINE isClosed #-}

close :: SomeBlob a -> IO ()
close b = js_close b
{-# INLINE close #-}

-- -----------------------------------------------------------------------------

js_size :: SomeBlob a -> Int
js_size = stub
js_type :: SomeBlob a -> JSString
js_type = stub

-- fixme figure out if we need to support older browsers with obsolete slice
js_slice :: Int -> Int -> JSString -> SomeBlob a -> SomeBlob a
js_slice = stub

js_isClosed :: SomeBlob a -> IO Bool
js_isClosed = stub

js_close :: SomeBlob a -> IO ()
js_close = stub




