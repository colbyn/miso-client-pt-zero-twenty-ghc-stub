{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}

{-# LANGUAGE NoPatternSynonyms #-}


module Data.JSString.RegExp
    ( RegExp
    
    -- NOTE the export named `pattern`
    -- May require 'NoPatternSynonyms'...
    , pattern
    , isMultiline
    , isIgnoreCase
    , Match(..)
    , REFlags(..)
    , create
    , test
    , exec
    , execNext
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
import GHCJS.Prim
import GHC.Exts (Any, Int#, Int(..))

import Unsafe.Coerce (unsafeCoerce)

import Data.JSString
import Data.Typeable



-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


newtype RegExp = RegExp JSVal deriving Typeable

data REFlags = REFlags { multiline  :: !Bool
                       , ignoreCase :: !Bool
                       }

data Match = Match { matched       :: !JSString  -- ^ the matched string
                   , subMatched    :: [JSString] -- ^ the matched parentesized substrings
                   , matchRawIndex :: !Int       -- ^ the raw index of the match in the string
                   , matchInput    :: !JSString  -- ^ the input string
                   }

create :: REFlags -> JSString -> RegExp
create flags pat = js_createRE pat flags'
  where
    flags' | multiline flags = if ignoreCase flags then "mi" else "m"
           | otherwise       = if ignoreCase flags then "i"  else ""
{-# INLINE create #-}

pattern :: RegExp -> JSString
pattern re = js_pattern re

isMultiline :: RegExp -> Bool
isMultiline re = js_isMultiline re

isIgnoreCase :: RegExp -> Bool
isIgnoreCase re = js_isIgnoreCase re

test :: JSString -> RegExp -> Bool
test x re = js_test x re
{-# INLINE test #-}

exec :: JSString -> RegExp -> Maybe Match
exec x re = exec' 0# x re
{-# INLINE exec #-}

execNext :: Match -> RegExp -> Maybe Match
execNext m re = case matchRawIndex m of
                  I# i -> exec' i (matchInput m) re
{-# INLINE execNext #-}

exec' :: Int# -> JSString -> RegExp -> Maybe Match
exec' i x re = case js_exec i x re of
                 (# -1#, _, _ #) -> Nothing
                 (# i',  y, z #) -> Just (Match y (unsafeCoerce z) (I# i) x)
{-# INLINE exec' #-}

matches :: JSString -> RegExp -> [Match]
matches x r = maybe [] go (exec x r)
  where
    go m = m : maybe [] go (execNext m r)
{-# INLINE matches #-}

replace :: RegExp -> JSString -> JSString -> JSString
replace x r = error "Data.JSString.RegExp.replace not implemented"
{-# INLINE replace #-}

split :: JSString -> RegExp -> [JSString]
split x r = unsafeCoerce (js_split -1# x r)
{-# INLINE split #-}

splitN :: Int -> JSString -> RegExp -> [JSString]
splitN (I# k) x r = unsafeCoerce (js_split k x r)
{-# INLINE splitN #-}

-- ----------------------------------------------------------------------------

js_createRE :: JSString -> JSString -> RegExp
js_createRE = stub

js_test :: JSString -> RegExp -> Bool
js_test = stub

js_exec :: Int# -> JSString -> RegExp -> (# Int#, JSString, Any {- [JSString] -} #)
js_exec = stub

js_replace :: RegExp -> JSString -> JSString -> JSString
js_replace = stub

js_split :: Int# -> JSString -> RegExp -> Any -- [JSString]
js_split = stub

js_isMultiline :: RegExp -> Bool
js_isMultiline = stub

js_isIgnoreCase :: RegExp -> Bool
js_isIgnoreCase = stub

js_pattern :: RegExp -> JSString
js_pattern = stub




