{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Internal
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Html.Internal
    (
    -- * Core types and interface
    VTree  (..)
    , View   (..)
    , ToView (..)
    , Attribute (..)
    -- * Smart `View` constructors
    , node
    , text
    -- * Key patch internals
    , Key    (..)
    , ToKey  (..)
    -- * Namespace
    , NS     (..)
    -- * Setting properties on virtual DOM nodes
    , prop
    -- * Setting css
    , style_
    -- * Handling events
    , on
    , onWithOptions
    -- * Life cycle events
    , onCreated
    , onDestroyed
    -- * Events
    , defaultEvents
    -- * Subscription type
    , Sub
    )
where


-- ~
import Core hiding (on)
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
    , Word(..)
    , pure
    , traverse
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
-- Miso Specific
-- ---------------
import           Control.Monad
import           Data.Aeson.Types           (parseEither)
import           Data.JSString
import qualified Data.Map                   as M
import           Data.Monoid
import           Data.Proxy
import           Data.String                (IsString(..))
import qualified Data.Text                  as T
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Object
import           JavaScript.Object.Internal (Object (Object))
import qualified JavaScript.Array as JSArray
import           Servant.API

import           Miso.Effect (Sub)
import           Miso.Event.Decoder
import           Miso.Event.Types
import           Miso.String
import           Miso.Effect (Sink)
import           Miso.FFI



-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~




-- | Virtual DOM implemented as a JavaScript `Object`.
--   Used for diffing, patching and event delegation.
--   Not meant to be constructed directly, see `View` instead.
newtype VTree = VTree { getTree :: Object }

-- | Core type for constructing a `VTree`, use this instead of `VTree` directly.
newtype View action = View {
  runView :: Sink action -> IO VTree
} deriving Functor

-- | For constructing type-safe links
instance HasLink (View a) where
  type MkLink (View a) = MkLink (Get '[] ())
  toLink _ = toLink (Proxy :: Proxy (Get '[] ()))

-- | Convenience class for using View
class ToView v where toView :: v -> View m

-- | `ToJSVal` instance for `Decoder`
instance ToJSVal DecodeTarget where
  toJSVal (DecodeTarget xs) = toJSVal xs
  toJSVal (DecodeTargets xs) = toJSVal xs

-- | Create a new @VNode@.
--
-- @node ns tag key attrs children@ creates a new node with tag @tag@
-- and 'Key' @key@ in the namespace @ns@. All @attrs@ are called when
-- the node is created and its children are initialized to @children@.
node :: NS
     -> MisoString
     -> Maybe Key
     -> [Attribute m]
     -> [View m]
     -> View m
node ns tag key attrs kids = View $ \sink -> do
  vnode <- create
  cssObj <- jsval <$> create
  propsObj <- jsval <$> create
  eventObj <- jsval <$> create
  set "css" cssObj vnode
  set "props" propsObj vnode
  set "events" eventObj vnode
  set "type" ("vnode" :: JSString) vnode
  set "ns" ns vnode
  set "tag" tag vnode
  set "key" key vnode
  setAttrs vnode sink
  flip (set "children") vnode =<< setKids sink
  pure $ VTree vnode
    where
      setAttrs vnode sink =
        forM_ attrs $ \(Attribute attr) ->
          attr sink vnode

      setKids sink =
        jsval . JSArray.fromList <$>
          fmap (jsval . getTree) <$>
            traverse (flip runView sink) kids

instance ToJSVal Options
instance ToJSVal Key where toJSVal (Key x) = toJSVal x

instance ToJSVal NS where
  toJSVal SVG  = toJSVal ("svg" :: JSString)
  toJSVal HTML = toJSVal ("html" :: JSString)

-- | Namespace of DOM elements.
data NS
  = HTML -- ^ HTML Namespace
  | SVG  -- ^ SVG Namespace
  deriving (Show, Eq)

-- | Create a new @VText@ with the given content.
text :: MisoString -> View m
text t = View . const $ do
  vtree <- create
  set "type" ("vtext" :: JSString) vtree
  set "text" t vtree
  pure $ VTree vtree

-- | `IsString` instance
instance IsString (View a) where
  fromString = text . fromString

-- | A unique key for a dom node.
--
-- This key is only used to speed up diffing the children of a DOM
-- node, the actual content is not important. The keys of the children
-- of a given DOM node must be unique. Failure to satisfy this
-- invariant gives undefined behavior at runtime.
newtype Key = Key MisoString

-- | Convert custom key types to `Key`.
--
-- Instances of this class do not have to guarantee uniqueness of the
-- generated keys, it is up to the user to do so. `toKey` must be an
-- injective function.
class ToKey key where toKey :: key -> Key
-- | Identity instance
instance ToKey Key where toKey = id
-- | Convert `MisoString` to `Key`
instance ToKey MisoString where toKey = Key
-- | Convert `Text` to `Key`
instance ToKey T.Text where toKey = Key . toMisoString
-- | Convert `String` to `Key`
instance ToKey String where toKey = Key . toMisoString
-- | Convert `Int` to `Key`
instance ToKey Int where toKey = Key . toMisoString
-- | Convert `Double` to `Key`
instance ToKey Double where toKey = Key . toMisoString
-- | Convert `Float` to `Key`
instance ToKey Float where toKey = Key . toMisoString
-- | Convert `Word` to `Key`
instance ToKey Word where toKey = Key . toMisoString

-- | Attribute of a vnode in a `View`.
--
-- The 'Sink' callback can be used to dispatch actions which are fed back to
-- the @update@ function. This is especially useful for event handlers
-- like the @onclick@ attribute. The second argument represents the
-- vnode the attribute is attached to.
newtype Attribute action = Attribute (Sink action -> Object -> IO ())

-- | @prop k v@ is an attribute that will set the attribute @k@ of the DOM node associated with the vnode
-- to @v@.
prop :: ToJSVal a => MisoString -> a -> Attribute action
prop k v = Attribute . const $ \n -> do
  val <- toJSVal v
  o <- getProp ("props" :: MisoString) n
  set k val (Object o)

-- | Convenience wrapper for @onWithOptions defaultOptions@.
--
-- > let clickHandler = on "click" emptyDecoder $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
on :: MisoString
   -> Decoder r
   -> (r -> action)
   -> Attribute action
on = onWithOptions defaultOptions

-- | @onWithOptions opts eventName decoder toAction@ is an attribute
-- that will set the event handler of the associated DOM node to a function that
-- decodes its argument using @decoder@, converts it to an action
-- using @toAction@ and then feeds that action back to the @update@ function.
--
-- @opts@ can be used to disable further event propagation.
--
-- > let clickHandler = onWithOptions defaultOptions "click" emptyDecoder $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
onWithOptions
  :: Options
  -> MisoString
  -> Decoder r
  -> (r -> action)
  -> Attribute action
onWithOptions options eventName Decoder{..} toAction =
  Attribute $ \sink n -> do
   eventObj <- getProp "events" n
   eventHandlerObject@(Object eo) <- create
   jsOptions <- toJSVal options
   decodeAtVal <- toJSVal decodeAt
   cb <- asyncCallback1 $ \e -> do
       Just v <- jsvalToValue =<< objectToJSON decodeAtVal e
       case parseEither decoder v of
         Left s -> error $ "Parse error on " <> unpack eventName <> ": " <> s
         Right r -> sink (toAction r)
   setProp "runEvent" (jsval cb) eventHandlerObject
   registerCallback cb
   setProp "options" jsOptions eventHandlerObject
   setProp eventName eo (Object eventObj)

-- | @onCreated action@ is an event that gets called after the actual DOM
-- element is created.
onCreated :: action -> Attribute action
onCreated action =
  Attribute $ \sink n -> do
    cb <- asyncCallback (sink action)
    setProp "onCreated" (jsval cb) n
    registerCallback cb

-- | @onDestroyed action@ is an event that gets called after the DOM element
-- is removed from the DOM. The @action@ is given the DOM element that was
-- removed from the DOM tree.
onDestroyed :: action -> Attribute action
onDestroyed action =
  Attribute $ \sink n -> do
    cb <- asyncCallback (sink action)
    setProp "onDestroyed" (jsval cb) n
    registerCallback cb

-- | @style_ attrs@ is an attribute that will set the @style@
-- attribute of the associated DOM node to @attrs@.
--
-- @style@ attributes not contained in @attrs@ will be deleted.
--
-- > import qualified Data.Map as M
-- > div_ [ style_  $ M.singleton "background" "red" ] [ ]
--
-- <https://developer.mozilla.org/en-US/docs/Web/CSS>
--
style_ :: M.Map MisoString MisoString -> Attribute action
style_ m = Attribute . const $ \n -> do
   cssObj <- getProp "css" n
   forM_ (M.toList m) $ \(k,v) ->
     setProp k (jsval v) (Object cssObj)

registerCallback
    :: Callback a
    -> IO ()
registerCallback = stub



