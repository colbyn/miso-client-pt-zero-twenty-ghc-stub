{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP, DeriveGeneric, LambdaCase, MagicHash, StandaloneDeriving #-}
module GHCJS.Prim.TH.Types (
    Message(..)
  , THResultType(..)
) where


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
    , pure
    , FilePath
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

import           Control.Applicative

import           Data.Binary
import           Data.ByteString (ByteString)
import           Data.Word

import           GHC.Generics
import           GHC.Exts

import           GHCJS.Prim.TH.Serialized

import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~



data Message
  -- | compiler to node requests
  = RunTH THResultType ByteString (Maybe TH.Loc)
  | FinishTH
  -- | node to compiler responses
  | RunTH'            ByteString -- ^ serialized result
  | FinishTH'
  -- | node to compiler requests
  | NewName           String
  | Report            Bool String
  | LookupName        Bool String
  | Reify             TH.Name
  | ReifyInstances    TH.Name [TH.Type]
  | ReifyRoles        TH.Name
  | ReifyAnnotations  TH.AnnLookup
  | ReifyModule       TH.Module
  | AddDependentFile  FilePath
  | AddTopDecls       [TH.Dec]
  -- | compiler to node responses
  | NewName'          TH.Name
  | Report'
  | LookupName'       (Maybe TH.Name)
  | Reify'            TH.Info
  | ReifyInstances'   [TH.Dec]
  | ReifyRoles'       [TH.Role]
  | ReifyAnnotations' [ByteString]
  | ReifyModule'      TH.ModuleInfo
  | AddDependentFile'
  | AddTopDecls'
  -- | exit with error status
  | QFail             String
  | QException        String
  deriving (Generic)


data THResultType = THExp | THPat | THType | THDec | THAnnWrapper
  deriving (Enum, Generic)



