{-# LANGUAGE NoImplicitPrelude #-}
module Core (
      module Proto.Data
    , module Proto.Class
    , module Proto.Util
    , module Proto.System.Console
    , stub
)
where


import Proto.Data
import Proto.Util
import Proto.Class
import Proto.System.Console
import qualified Prelude


-- For Stubs
stub :: a
stub = Prelude.error "Attempted evaluation of a ghc-js stub."


