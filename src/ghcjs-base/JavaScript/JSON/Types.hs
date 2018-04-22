{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DeriveDataTypeable, DefaultSignatures, FlexibleContexts #-}
module JavaScript.JSON.Types
    ( Value
    , Object
    , Options(..)
    , defaultOptions
    , SumEncoding(..)
    , defaultTaggedObject
    , camelTo
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
    , undefined
    , fail
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
import Control.DeepSeq

import Data.Aeson.Types (Options(..), SumEncoding(..))

import Data.Maybe
import Data.Typeable
import Data.JSString

import GHC.Generics

import           JavaScript.JSON.Types.Class
import           JavaScript.JSON.Types.Internal
import qualified JavaScript.JSON.Types.Internal as I


-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~




runParser = undefined

-- | Run a 'Parser'.
parse :: (a -> Parser b) -> a -> Result b
parse m v = runParser (m v) Error Success
{-# INLINE parse #-}

-- | Run a 'Parser' with a 'Maybe' result type.
parseMaybe :: (a -> Parser b) -> a -> Maybe b
parseMaybe m v = runParser (m v) (const Nothing) Just
{-# INLINE parseMaybe #-}

-- | Run a 'Parser' with an 'Either' result type.
parseEither :: (a -> Parser b) -> a -> Either String b
parseEither m v = runParser (m v) Left Right
{-# INLINE parseEither #-}

-- | If the inner @Parser@ failed, modify the failure message using the
-- provided function. This allows you to create more descriptive error messages.
-- For example:
--
-- > parseJSON (Object o) = modifyFailure
-- >     ("Parsing of the Foo value failed: " ++)
-- >     (Foo <$> o .: "someField")
--
-- Since 0.6.2.0
modifyFailure :: (JSString -> JSString) -> Parser a -> Parser a
modifyFailure = undefined -- f (Parser p) = Parser $ \kf -> p (kf . f)

-- | Construct a 'Pair' from a key and a value.
(.=) :: ToJSON a => JSString -> a -> Pair
name .= value = (name, toJSON value)
{-# INLINE (.=) #-}

-- | Convert a value from JSON, failing if the types do not match.
fromJSON :: (FromJSON a) => Value -> Result a
fromJSON = I.parse parseJSON
{-# INLINE fromJSON #-}

(.:) :: FromJSON a => Object -> JSString -> Parser a
obj .: key = case I.lookup key obj of
               Nothing -> fail $ "key " ++ show key ++ " not present"
               Just v  -> parseJSON v
{-# INLINE (.:) #-}

(.:?) :: FromJSON a => Object -> JSString -> Parser (Maybe a)
obj .:? key = case I.lookup key obj of
                Nothing -> pure Nothing
                Just v  -> Just <$> parseJSON v
{-# INLINE (.:?) #-}

(.!=) :: Parser (Maybe a) -> a -> Parser a
pmval .!= val = fromMaybe val <$> pmval
{-# INLINE (.!=) #-}
