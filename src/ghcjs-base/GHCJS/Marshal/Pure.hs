{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
module GHCJS.Marshal.Pure (
    PFromJSVal(..)
  , PToJSVal(..)
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
import           Data.Char (chr, ord)
import           Data.Data
import           Data.Int (Int8, Int16, Int32)
import           Data.JSString.Internal.Type
import           Data.Maybe
import           Data.Text (Text)
import           Data.Typeable
import           Data.Word (Word8, Word16, Word32, Word)
import           Data.JSString
import           Data.JSString.Text
import           Data.Bits ((.&.))
import           Unsafe.Coerce (unsafeCoerce)
import           GHC.Int
import           GHC.Word
import           GHC.Types
import           GHC.Float
import           GHC.Prim

import           GHCJS.Types
import qualified GHCJS.Prim as Prim
import           GHCJS.Foreign.Internal
import           GHCJS.Marshal.Internal



-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~


{-
type family IsPureShared a where
  IsPureShared PureExclusive = False
  IsPureShared PureShared    = True

type family IsPureExclusive a where
  IsPureExclusive PureExclusive = True
  IsPureExclusive PureShared    = True
  -}

instance PFromJSVal JSVal where pFromJSVal = id
                                {-# INLINE pFromJSVal #-}
instance PFromJSVal ()    where pFromJSVal _ = ()
                                {-# INLINE pFromJSVal #-}

instance PFromJSVal JSString where pFromJSVal = JSString
                                   {-# INLINE pFromJSVal #-}
instance PFromJSVal [Char] where pFromJSVal   = Prim.fromJSString
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Text   where pFromJSVal   = stub
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Char   where pFromJSVal x = C# (jsvalToChar x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Bool   where pFromJSVal   = stub
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Int    where pFromJSVal x = I# (jsvalToInt x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Int8   where pFromJSVal x = I8# (jsvalToInt8 x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Int16  where pFromJSVal x = I16# (jsvalToInt16 x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Int32  where pFromJSVal x = I32# (jsvalToInt x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Word   where pFromJSVal x = W# (jsvalToWord x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Word8  where pFromJSVal x = W8# (jsvalToWord8 x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Word16 where pFromJSVal x = W16# (jsvalToWord16 x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Word32 where pFromJSVal x = W32# (jsvalToWord x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Float  where pFromJSVal x = F# (jsvalToFloat x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Double where pFromJSVal x = D# (jsvalToDouble x)
                                 {-# INLINE pFromJSVal #-}

instance PFromJSVal a => PFromJSVal (Maybe a) where
    pFromJSVal x | isUndefined x || isNull x = Nothing
    pFromJSVal x = Just (pFromJSVal x)
    {-# INLINE pFromJSVal #-}

instance PToJSVal JSVal     where pToJSVal = id
                                  {-# INLINE pToJSVal #-}
instance PToJSVal JSString  where pToJSVal          = jsval
                                  {-# INLINE pToJSVal #-}
instance PToJSVal [Char]    where pToJSVal          = Prim.toJSString
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Text      where pToJSVal          = stub
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Char      where pToJSVal (C# c)   = charToJSVal c
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Bool      where pToJSVal True     = stub
                                  pToJSVal False    = stub
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Int       where pToJSVal (I# x)   = intToJSVal x
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Int8      where pToJSVal (I8# x)  = intToJSVal x
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Int16     where pToJSVal (I16# x) = intToJSVal x
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Int32     where pToJSVal (I32# x) = intToJSVal x
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Word      where pToJSVal (W# x)   = wordToJSVal x
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Word8     where pToJSVal (W8# x)  = wordToJSVal x
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Word16    where pToJSVal (W16# x) = wordToJSVal x
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Word32    where pToJSVal (W32# x) = wordToJSVal x
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Float     where pToJSVal (F# x)   = floatToJSVal x
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Double    where pToJSVal (D# x)   = doubleToJSVal x
                                  {-# INLINE pToJSVal #-}

instance PToJSVal a => PToJSVal (Maybe a) where
    pToJSVal Nothing  = stub
    pToJSVal (Just a) = stub
    {-# INLINE pToJSVal #-}

jsvalToWord   :: JSVal -> Word#
jsvalToWord = stub

jsvalToWord8  :: JSVal -> Word#
jsvalToWord8 = stub

jsvalToWord16 :: JSVal -> Word#
jsvalToWord16 = stub

jsvalToInt    :: JSVal -> Int#
jsvalToInt = stub

jsvalToInt8   :: JSVal -> Int#
jsvalToInt8 = stub

jsvalToInt16  :: JSVal -> Int#
jsvalToInt16 = stub

jsvalToFloat  :: JSVal -> Float#
jsvalToFloat = stub

jsvalToDouble :: JSVal -> Double#
jsvalToDouble = stub

jsvalToChar   :: JSVal -> Char#
jsvalToChar = stub

wordToJSVal   :: Word#   -> JSVal
wordToJSVal = stub

intToJSVal    :: Int#    -> JSVal
intToJSVal = stub

doubleToJSVal :: Double# -> JSVal
doubleToJSVal = stub

floatToJSVal  :: Float#  -> JSVal
floatToJSVal = stub

charToJSVal   :: Char#   -> JSVal
charToJSVal = stub





