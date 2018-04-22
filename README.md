# miso-client-pt-zero-twenty-ghc-stub
> Also contains stubs for ghcjs base and ghcjs prims libraries.


## Quick View of Coverage

```
src/
├── ghcjs-base
│   ├── Data
│   │   ├── JSString
│   │   │   ├── Int.hs
│   │   │   ├── Internal
│   │   │   │   ├── Fusion
│   │   │   │   │   ├── CaseMapping.hs
│   │   │   │   │   ├── Common.hs
│   │   │   │   │   └── Types.hs
│   │   │   │   ├── Fusion.hs
│   │   │   │   ├── Search.hs
│   │   │   │   └── Type.hs
│   │   │   ├── Internal.hs
│   │   │   ├── Raw.hs
│   │   │   ├── Read.hs
│   │   │   ├── RealFloat.hs
│   │   │   ├── RegExp.hs
│   │   │   └── Text.hs
│   │   └── JSString.hs
│   ├── GHCJS
│   │   ├── Buffer
│   │   │   └── Types.hs
│   │   ├── Buffer.hs
│   │   ├── Concurrent.hs
│   │   ├── Foreign
│   │   │   ├── Callback
│   │   │   │   └── Internal.hs
│   │   │   ├── Callback.hs
│   │   │   ├── Export.hs
│   │   │   └── Internal.hs
│   │   ├── Foreign.hs
│   │   ├── Internal
│   │   │   └── Types.hs
│   │   ├── Marshal
│   │   │   ├── Internal.hs
│   │   │   └── Pure.hs
│   │   ├── Marshal.hs
│   │   ├── Nullable.hs
│   │   └── Types.hs
│   └── JavaScript
│       ├── Array
│       │   ├── Immutable.hs
│       │   ├── Internal.hs
│       │   └── ST.hs
│       ├── Array.hs
│       ├── Cast.hs
│       ├── JSON
│       │   ├── Types
│       │   │   ├── Class.hs
│       │   │   ├── Generic.hs
│       │   │   ├── Instances.hs
│       │   │   └── Internal.hs
│       │   └── Types.hs
│       ├── JSON.hs
│       ├── Number.hs
│       ├── Object
│       │   └── Internal.hs
│       ├── Object.hs
│       ├── RegExp.hs
│       ├── String.hs
│       ├── TypedArray
│       │   ├── ArrayBuffer
│       │   │   ├── Internal.hs
│       │   │   ├── ST.hs
│       │   │   └── Type.hs
│       │   ├── ArrayBuffer.hs
│       │   ├── DataView
│       │   │   ├── Internal.hs
│       │   │   └── ST.hs
│       │   ├── DataView.hs
│       │   ├── Immutable.hs
│       │   ├── Internal
│       │   │   └── Types.hs
│       │   ├── Internal.hs
│       │   └── ST.hs
│       ├── TypedArray.hs
│       └── Web
│           ├── AnimationFrame.hs
│           ├── Blob
│           │   └── Internal.hs
│           ├── Blob.hs
│           ├── Canvas
│           │   ├── ImageData.hs
│           │   ├── Internal.hs
│           │   ├── Pattern.hs
│           │   ├── TextMetrics.hs
│           │   └── Types.hs
│           ├── Canvas.hs
│           ├── CloseEvent
│           │   └── Internal.hs
│           ├── CloseEvent.hs
│           ├── ErrorEvent
│           │   └── Internal.hs
│           ├── ErrorEvent.hs
│           ├── File.hs
│           ├── FileReader.hs
│           ├── History.hs
│           ├── Location.hs
│           ├── MessageEvent
│           │   └── Internal.hs
│           ├── MessageEvent.hs
│           ├── Performance.hs
│           ├── Storage
│           │   └── Internal.hs
│           ├── Storage.hs
│           ├── StorageEvent.hs
│           ├── WebSocket.hs
│           ├── Worker
│           │   └── Haskell.hs
│           ├── Worker.hs
│           └── XMLHttpRequest.hs
├── ghcjs-prim
│   └── GHCJS
│       ├── Prim
│       │   ├── Internal
│       │   │   └── Build.hs
│       │   ├── Internal.hs
│       │   └── TH
│       │       ├── Eval.hs
│       │       ├── Serialized.hs
│       │       └── Types.hs
│       └── Prim.hs
├── miso-ghcjs-client
│   └── Miso
│       ├── Delegate.hs
│       ├── Dev.hs
│       ├── Diff.hs
│       ├── Effect
│       │   ├── DOM.hs
│       │   ├── Storage.hs
│       │   └── XHR.hs
│       ├── Effect.hs
│       ├── FFI.hs
│       ├── Html
│       │   └── Internal.hs
│       ├── String.hs
│       ├── Subscription
│       │   ├── History.hs
│       │   ├── Keyboard.hs
│       │   ├── Mouse.hs
│       │   ├── SSE.hs
│       │   ├── WebSocket.hs
│       │   └── Window.hs
│       ├── Subscription.hs
│       └── Types.hs
└── miso-shared
    └── Miso
        ├── Concurrent.hs
        ├── Event
        │   ├── Decoder.hs
        │   └── Types.hs
        ├── Event.hs
        ├── Html
        │   ├── Element.hs
        │   ├── Event.hs
        │   └── Property.hs
        ├── Html.hs
        ├── Router.hs
        ├── Svg
        │   ├── Attribute.hs
        │   ├── Element.hs
        │   └── Event.hs
        ├── Svg.hs
        └── Util.hs
```



