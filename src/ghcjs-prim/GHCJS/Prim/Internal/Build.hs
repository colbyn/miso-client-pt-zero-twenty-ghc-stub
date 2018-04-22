{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
module GHCJS.Prim.Internal.Build
    -- ( buildArrayI
    -- , buildArrayM
    -- , buildObjectI
    -- , buildObjectM
    -- , buildArrayI1
    -- , buildArrayI2
    -- , buildArrayI3
    -- , buildArrayI4
    -- , buildArrayI5
    -- , buildArrayI6
    -- , buildArrayI7
    -- , buildArrayI8
    -- , buildArrayI9
    -- , buildArrayI10
    -- , buildArrayI11
    -- , buildArrayI12
    -- , buildArrayI13
    -- , buildArrayI14
    -- , buildArrayI15
    -- , buildArrayI16
    -- , buildArrayI17
    -- , buildArrayI18
    -- , buildArrayI19
    -- , buildArrayI20
    -- , buildArrayI21
    -- , buildArrayI22
    -- , buildArrayI23
    -- , buildArrayI24
    -- , buildArrayI25
    -- , buildArrayI26
    -- , buildArrayI27
    -- , buildArrayI28
    -- , buildArrayI29
    -- , buildArrayI30
    -- , buildArrayI31
    -- , buildArrayI32
    -- , buildArrayM1
    -- , buildArrayM2
    -- , buildArrayM3
    -- , buildArrayM4
    -- , buildArrayM5
    -- , buildArrayM6
    -- , buildArrayM7
    -- , buildArrayM8
    -- , buildArrayM9
    -- , buildArrayM10
    -- , buildArrayM11
    -- , buildArrayM12
    -- , buildArrayM13
    -- , buildArrayM14
    -- , buildArrayM15
    -- , buildArrayM16
    -- , buildArrayM17
    -- , buildArrayM18
    -- , buildArrayM19
    -- , buildArrayM20
    -- , buildArrayM21
    -- , buildArrayM22
    -- , buildArrayM23
    -- , buildArrayM24
    -- , buildArrayM25
    -- , buildArrayM26
    -- , buildArrayM27
    -- , buildArrayM28
    -- , buildArrayM29
    -- , buildArrayM30
    -- , buildArrayM31
    -- , buildArrayM32
    -- , buildObjectI1
    -- , buildObjectI2
    -- , buildObjectI3
    -- , buildObjectI4
    -- , buildObjectI5
    -- , buildObjectI6
    -- , buildObjectI7
    -- , buildObjectI8
    -- , buildObjectI9
    -- , buildObjectI10
    -- , buildObjectI11
    -- , buildObjectI12
    -- , buildObjectI13
    -- , buildObjectI14
    -- , buildObjectI15
    -- , buildObjectI16
    -- , buildObjectI17
    -- , buildObjectI18
    -- , buildObjectI19
    -- , buildObjectI20
    -- , buildObjectI21
    -- , buildObjectI22
    -- , buildObjectI23
    -- , buildObjectI24
    -- , buildObjectI25
    -- , buildObjectI26
    -- , buildObjectI27
    -- , buildObjectI28
    -- , buildObjectI29
    -- , buildObjectI30
    -- , buildObjectI31
    -- , buildObjectI32
    -- , buildObjectM1
    -- , buildObjectM2
    -- , buildObjectM3
    -- , buildObjectM4
    -- , buildObjectM5
    -- , buildObjectM6
    -- , buildObjectM7
    -- , buildObjectM8
    -- , buildObjectM9
    -- , buildObjectM10
    -- , buildObjectM11
    -- , buildObjectM12
    -- , buildObjectM13
    -- , buildObjectM14
    -- , buildObjectM15
    -- , buildObjectM16
    -- , buildObjectM17
    -- , buildObjectM18
    -- , buildObjectM19
    -- , buildObjectM20
    -- , buildObjectM21
    -- , buildObjectM22
    -- , buildObjectM23
    -- , buildObjectM24
    -- , buildObjectM25
    -- , buildObjectM26
    -- , buildObjectM27
    -- , buildObjectM28
    -- , buildObjectM29
    -- , buildObjectM30
    -- , buildObjectM31
    -- , buildObjectM32
    -- )
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
import GHC.Prim
import Unsafe.Coerce
import System.IO.Unsafe
import Data.Typeable (Typeable)
-- --------------
-- Project Scope
-- ---------------
-- + Local
-- ~



-- NOTE: Added this:
data Any = forall a. (Typeable a) => Any a


type O = JSVal -- object
type K = JSVal -- key
type V = JSVal -- value
type J = JSVal -- some JS value
type A = JSVal -- array

seqTupList :: [(a,b)] -> [(a,b)]
seqTupList xs = go xs `seq` xs
  where go ((x1,x2):xs) = x1 `seq` x2 `seq` go xs
        go []           = ()

js_emptyArrayI :: A
js_emptyArrayI = stub

buildArrayI :: [J] -> A
buildArrayI xs = unsafePerformIO (toJSArray xs)
{-# INLINE [1] buildArrayI #-}
{-# RULES "buildArrayI/empty" buildArrayI [] = js_emptyArrayI #-}
{-# RULES "buildArrayI/buildArrayI1" forall x1. buildArrayI [x1] = buildArrayI1 x1 #-}
{-# RULES "buildArrayI/buildArrayI2" forall x1 x2. buildArrayI [x1,x2] = buildArrayI2 x1 x2 #-}
{-# RULES "buildArrayI/buildArrayI3" forall x1 x2 x3. buildArrayI [x1,x2,x3] = buildArrayI3 x1 x2 x3 #-}
{-# RULES "buildArrayI/buildArrayI4" forall x1 x2 x3 x4. buildArrayI [x1,x2,x3,x4] = buildArrayI4 x1 x2 x3 x4 #-}
{-# RULES "buildArrayI/buildArrayI5" forall x1 x2 x3 x4 x5. buildArrayI [x1,x2,x3,x4,x5] = buildArrayI5 x1 x2 x3 x4 x5 #-}
{-# RULES "buildArrayI/buildArrayI6" forall x1 x2 x3 x4 x5 x6. buildArrayI [x1,x2,x3,x4,x5,x6] = buildArrayI6 x1 x2 x3 x4 x5 x6 #-}
{-# RULES "buildArrayI/buildArrayI7" forall x1 x2 x3 x4 x5 x6 x7. buildArrayI [x1,x2,x3,x4,x5,x6,x7] = buildArrayI7 x1 x2 x3 x4 x5 x6 x7 #-}
{-# RULES "buildArrayI/buildArrayI8" forall x1 x2 x3 x4 x5 x6 x7 x8. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8] = buildArrayI8 x1 x2 x3 x4 x5 x6 x7 x8 #-}
{-# RULES "buildArrayI/buildArrayI9" forall x1 x2 x3 x4 x5 x6 x7 x8 x9. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9] = buildArrayI9 x1 x2 x3 x4 x5 x6 x7 x8 x9 #-}
{-# RULES "buildArrayI/buildArrayI10" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10] = buildArrayI10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 #-}
{-# RULES "buildArrayI/buildArrayI11" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11] = buildArrayI11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 #-}
{-# RULES "buildArrayI/buildArrayI12" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12] = buildArrayI12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 #-}
{-# RULES "buildArrayI/buildArrayI13" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13] = buildArrayI13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 #-}
{-# RULES "buildArrayI/buildArrayI14" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14] = buildArrayI14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 #-}
{-# RULES "buildArrayI/buildArrayI15" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15] = buildArrayI15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 #-}
{-# RULES "buildArrayI/buildArrayI16" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16] = buildArrayI16 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 #-}
{-# RULES "buildArrayI/buildArrayI17" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17] = buildArrayI17 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 #-}
{-# RULES "buildArrayI/buildArrayI18" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18] = buildArrayI18 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 #-}
{-# RULES "buildArrayI/buildArrayI19" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19] = buildArrayI19 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 #-}
{-# RULES "buildArrayI/buildArrayI20" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20] = buildArrayI20 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 #-}
{-# RULES "buildArrayI/buildArrayI21" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21] = buildArrayI21 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 #-}
{-# RULES "buildArrayI/buildArrayI22" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22] = buildArrayI22 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 #-}
{-# RULES "buildArrayI/buildArrayI23" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23] = buildArrayI23 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 #-}
{-# RULES "buildArrayI/buildArrayI24" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24] = buildArrayI24 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 #-}
{-# RULES "buildArrayI/buildArrayI25" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25] = buildArrayI25 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 #-}
{-# RULES "buildArrayI/buildArrayI26" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26] = buildArrayI26 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 #-}
{-# RULES "buildArrayI/buildArrayI27" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27] = buildArrayI27 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 #-}
{-# RULES "buildArrayI/buildArrayI28" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28] = buildArrayI28 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 #-}
{-# RULES "buildArrayI/buildArrayI29" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29] = buildArrayI29 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 #-}
{-# RULES "buildArrayI/buildArrayI30" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30] = buildArrayI30 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 #-}
{-# RULES "buildArrayI/buildArrayI31" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31] = buildArrayI31 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 #-}
{-# RULES "buildArrayI/buildArrayI32" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32. buildArrayI [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,x32] = buildArrayI32 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 #-}

buildArrayI1 :: J -> A
buildArrayI1 x1 =
  js_buildArrayI1 x1
{-# INLINE buildArrayI1 #-}

js_buildArrayI1 :: J -> A
js_buildArrayI1 = stub


buildArrayI2 :: J -> J -> A
buildArrayI2 x1 x2 =
  js_buildArrayI2 x1 x2
{-# INLINE buildArrayI2 #-}

js_buildArrayI2 :: J -> J -> A
js_buildArrayI2 = stub


buildArrayI3 :: J -> J -> J -> A
buildArrayI3 x1 x2 x3 =
  js_buildArrayI3 x1 x2 x3
{-# INLINE buildArrayI3 #-}

js_buildArrayI3 :: J -> J -> J -> A
js_buildArrayI3 = stub


buildArrayI4 :: J -> J -> J -> J -> A
buildArrayI4 x1 x2 x3 x4 =
  js_buildArrayI4 x1 x2 x3 x4
{-# INLINE buildArrayI4 #-}

js_buildArrayI4 :: J -> J -> J -> J -> A
js_buildArrayI4 = stub


buildArrayI5 :: J -> J -> J -> J -> J -> A
buildArrayI5 x1 x2 x3 x4 x5 =
  js_buildArrayI5 x1 x2 x3 x4 x5
{-# INLINE buildArrayI5 #-}

js_buildArrayI5 :: J -> J -> J -> J -> J -> A
js_buildArrayI5 = stub


buildArrayI6 :: J -> J -> J -> J -> J -> J -> A
buildArrayI6 x1 x2 x3 x4 x5 x6 =
  js_buildArrayI6 x1 x2 x3 x4 x5 x6
{-# INLINE buildArrayI6 #-}

js_buildArrayI6 :: J -> J -> J -> J -> J -> J -> A
js_buildArrayI6 = stub


buildArrayI7 :: J -> J -> J -> J -> J -> J -> J -> A
buildArrayI7 x1 x2 x3 x4 x5 x6 x7 =
  js_buildArrayI7 x1 x2 x3 x4 x5 x6 x7
{-# INLINE buildArrayI7 #-}

js_buildArrayI7 :: J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI7 = stub


buildArrayI8 :: J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI8 x1 x2 x3 x4 x5 x6 x7 x8 =
  js_buildArrayI8 x1 x2 x3 x4 x5 x6 x7 x8
{-# INLINE buildArrayI8 #-}

js_buildArrayI8 :: J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI8 = stub


buildArrayI9 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI9 x1 x2 x3 x4 x5 x6 x7 x8 x9 =
  js_buildArrayI9 x1 x2 x3 x4 x5 x6 x7 x8 x9
{-# INLINE buildArrayI9 #-}

js_buildArrayI9 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI9 = stub


buildArrayI10 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 =
  js_buildArrayI10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
{-# INLINE buildArrayI10 #-}

js_buildArrayI10 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI10 = stub


buildArrayI11 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 =
  js_buildArrayI11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11
{-# INLINE buildArrayI11 #-}

js_buildArrayI11 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI11 = stub


buildArrayI12 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 =
  js_buildArrayI12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12
{-# INLINE buildArrayI12 #-}

js_buildArrayI12 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI12 = stub


buildArrayI13 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 =
  js_buildArrayI13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
{-# INLINE buildArrayI13 #-}

js_buildArrayI13 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI13 = stub


buildArrayI14 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 =
  js_buildArrayI14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
{-# INLINE buildArrayI14 #-}

js_buildArrayI14 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI14 = stub


buildArrayI15 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 =
  js_buildArrayI15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
{-# INLINE buildArrayI15 #-}

js_buildArrayI15 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI15 = stub


buildArrayI16 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI16 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 =
  js_buildArrayI16 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16
{-# INLINE buildArrayI16 #-}

js_buildArrayI16 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI16 = stub


buildArrayI17 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI17 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 =
  js_buildArrayI17 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17
{-# INLINE buildArrayI17 #-}

js_buildArrayI17 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI17 = stub


buildArrayI18 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI18 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 =
  js_buildArrayI18 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18
{-# INLINE buildArrayI18 #-}

js_buildArrayI18 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI18 = stub


buildArrayI19 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI19 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 =
  js_buildArrayI19 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19
{-# INLINE buildArrayI19 #-}

js_buildArrayI19 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI19 = stub


buildArrayI20 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI20 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 =
  js_buildArrayI20 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
{-# INLINE buildArrayI20 #-}

js_buildArrayI20 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI20 = stub

buildArrayI21 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI21 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 =
  js_buildArrayI21 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21
{-# INLINE buildArrayI21 #-}

js_buildArrayI21 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI21 = stub

buildArrayI22 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI22 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 =
  js_buildArrayI22 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22
{-# INLINE buildArrayI22 #-}

js_buildArrayI22 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI22 = stub

buildArrayI23 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI23 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 =
  js_buildArrayI23 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23
{-# INLINE buildArrayI23 #-}

js_buildArrayI23 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI23 = stub

buildArrayI24 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI24 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 =
  js_buildArrayI24 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24
{-# INLINE buildArrayI24 #-}

js_buildArrayI24 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI24 = stub

buildArrayI25 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI25 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 =
  js_buildArrayI25 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25
{-# INLINE buildArrayI25 #-}

js_buildArrayI25 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI25 = stub

buildArrayI26 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI26 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 =
  js_buildArrayI26 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
{-# INLINE buildArrayI26 #-}

js_buildArrayI26 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI26 = stub

buildArrayI27 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI27 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 =
  js_buildArrayI27 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27
{-# INLINE buildArrayI27 #-}

js_buildArrayI27 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI27 = stub

buildArrayI28 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI28 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 =
  js_buildArrayI28 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28
{-# INLINE buildArrayI28 #-}

js_buildArrayI28 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI28 = stub

buildArrayI29 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI29 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 =
  js_buildArrayI29 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29
{-# INLINE buildArrayI29 #-}

js_buildArrayI29 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI29 = stub

buildArrayI30 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI30 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 =
  js_buildArrayI30 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
{-# INLINE buildArrayI30 #-}

js_buildArrayI30 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI30 = stub

buildArrayI31 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI31 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 =
  js_buildArrayI31 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
{-# INLINE buildArrayI31 #-}

js_buildArrayI31 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI31 = stub

buildArrayI32 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
buildArrayI32 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 =
  js_buildArrayI32 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32
{-# INLINE buildArrayI32 #-}

js_buildArrayI32 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> A
js_buildArrayI32 = stub

js_emptyArrayM :: IO A
js_emptyArrayM = stub

buildArrayM :: [J] -> IO A
buildArrayM xs = toJSArray xs
{-# INLINE [1] buildArrayM #-}
{-# RULES "buildArrayM/empty" buildArrayM [] = js_emptyArrayM #-}
{-# RULES "buildArrayM/buildArrayM1" forall x1. buildArrayM [x1] = buildArrayM1 x1 #-}
{-# RULES "buildArrayM/buildArrayM2" forall x1 x2. buildArrayM [x1,x2] = buildArrayM2 x1 x2 #-}
{-# RULES "buildArrayM/buildArrayM3" forall x1 x2 x3. buildArrayM [x1,x2,x3] = buildArrayM3 x1 x2 x3 #-}
{-# RULES "buildArrayM/buildArrayM4" forall x1 x2 x3 x4. buildArrayM [x1,x2,x3,x4] = buildArrayM4 x1 x2 x3 x4 #-}
{-# RULES "buildArrayM/buildArrayM5" forall x1 x2 x3 x4 x5. buildArrayM [x1,x2,x3,x4,x5] = buildArrayM5 x1 x2 x3 x4 x5 #-}
{-# RULES "buildArrayM/buildArrayM6" forall x1 x2 x3 x4 x5 x6. buildArrayM [x1,x2,x3,x4,x5,x6] = buildArrayM6 x1 x2 x3 x4 x5 x6 #-}
{-# RULES "buildArrayM/buildArrayM7" forall x1 x2 x3 x4 x5 x6 x7. buildArrayM [x1,x2,x3,x4,x5,x6,x7] = buildArrayM7 x1 x2 x3 x4 x5 x6 x7 #-}
{-# RULES "buildArrayM/buildArrayM8" forall x1 x2 x3 x4 x5 x6 x7 x8. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8] = buildArrayM8 x1 x2 x3 x4 x5 x6 x7 x8 #-}
{-# RULES "buildArrayM/buildArrayM9" forall x1 x2 x3 x4 x5 x6 x7 x8 x9. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9] = buildArrayM9 x1 x2 x3 x4 x5 x6 x7 x8 x9 #-}
{-# RULES "buildArrayM/buildArrayM10" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10] = buildArrayM10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 #-}
{-# RULES "buildArrayM/buildArrayM11" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11] = buildArrayM11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 #-}
{-# RULES "buildArrayM/buildArrayM12" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12] = buildArrayM12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 #-}
{-# RULES "buildArrayM/buildArrayM13" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13] = buildArrayM13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 #-}
{-# RULES "buildArrayM/buildArrayM14" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14] = buildArrayM14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 #-}
{-# RULES "buildArrayM/buildArrayM15" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15] = buildArrayM15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 #-}
{-# RULES "buildArrayM/buildArrayM16" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16] = buildArrayM16 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 #-}
{-# RULES "buildArrayM/buildArrayM17" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17] = buildArrayM17 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 #-}
{-# RULES "buildArrayM/buildArrayM18" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18] = buildArrayM18 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 #-}
{-# RULES "buildArrayM/buildArrayM19" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19] = buildArrayM19 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 #-}
{-# RULES "buildArrayM/buildArrayM20" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20] = buildArrayM20 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 #-}
{-# RULES "buildArrayM/buildArrayM21" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21] = buildArrayM21 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 #-}
{-# RULES "buildArrayM/buildArrayM22" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22] = buildArrayM22 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 #-}
{-# RULES "buildArrayM/buildArrayM23" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23] = buildArrayM23 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 #-}
{-# RULES "buildArrayM/buildArrayM24" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24] = buildArrayM24 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 #-}
{-# RULES "buildArrayM/buildArrayM25" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25] = buildArrayM25 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 #-}
{-# RULES "buildArrayM/buildArrayM26" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26] = buildArrayM26 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 #-}
{-# RULES "buildArrayM/buildArrayM27" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27] = buildArrayM27 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 #-}
{-# RULES "buildArrayM/buildArrayM28" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28] = buildArrayM28 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 #-}
{-# RULES "buildArrayM/buildArrayM29" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29] = buildArrayM29 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 #-}
{-# RULES "buildArrayM/buildArrayM30" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30] = buildArrayM30 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 #-}
{-# RULES "buildArrayM/buildArrayM31" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31] = buildArrayM31 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 #-}
{-# RULES "buildArrayM/buildArrayM32" forall x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32. buildArrayM [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,x32] = buildArrayM32 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 #-}

buildArrayM1 :: J -> IO A
buildArrayM1 x1 =
  js_buildArrayM1 x1
{-# INLINE buildArrayM1 #-}

js_buildArrayM1 :: J -> IO A
js_buildArrayM1 = stub

buildArrayM2 :: J -> J -> IO A
buildArrayM2 x1 x2 =
  js_buildArrayM2 x1 x2
{-# INLINE buildArrayM2 #-}

js_buildArrayM2 :: J -> J -> IO A
js_buildArrayM2 = stub


buildArrayM3 :: J -> J -> J -> IO A
buildArrayM3 x1 x2 x3 =
  js_buildArrayM3 x1 x2 x3
{-# INLINE buildArrayM3 #-}

js_buildArrayM3 :: J -> J -> J -> IO A
js_buildArrayM3 = stub


buildArrayM4 :: J -> J -> J -> J -> IO A
buildArrayM4 x1 x2 x3 x4 =
  js_buildArrayM4 x1 x2 x3 x4
{-# INLINE buildArrayM4 #-}

js_buildArrayM4 :: J -> J -> J -> J -> IO A
js_buildArrayM4 = stub


buildArrayM5 :: J -> J -> J -> J -> J -> IO A
buildArrayM5 x1 x2 x3 x4 x5 =
  js_buildArrayM5 x1 x2 x3 x4 x5
{-# INLINE buildArrayM5 #-}

js_buildArrayM5 :: J -> J -> J -> J -> J -> IO A
js_buildArrayM5 = stub


buildArrayM6 :: J -> J -> J -> J -> J -> J -> IO A
buildArrayM6 x1 x2 x3 x4 x5 x6 =
  js_buildArrayM6 x1 x2 x3 x4 x5 x6
{-# INLINE buildArrayM6 #-}

js_buildArrayM6 :: J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM6 = stub


buildArrayM7 :: J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM7 x1 x2 x3 x4 x5 x6 x7 =
  js_buildArrayM7 x1 x2 x3 x4 x5 x6 x7
{-# INLINE buildArrayM7 #-}

js_buildArrayM7 :: J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM7 = stub


buildArrayM8 :: J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM8 x1 x2 x3 x4 x5 x6 x7 x8 =
  js_buildArrayM8 x1 x2 x3 x4 x5 x6 x7 x8
{-# INLINE buildArrayM8 #-}

js_buildArrayM8 :: J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM8 = stub


buildArrayM9 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM9 x1 x2 x3 x4 x5 x6 x7 x8 x9 =
  js_buildArrayM9 x1 x2 x3 x4 x5 x6 x7 x8 x9
{-# INLINE buildArrayM9 #-}

js_buildArrayM9 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM9 = stub


buildArrayM10 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 =
  js_buildArrayM10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
{-# INLINE buildArrayM10 #-}

js_buildArrayM10 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM10 = stub


buildArrayM11 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 =
  js_buildArrayM11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11
{-# INLINE buildArrayM11 #-}

js_buildArrayM11 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM11 = stub


buildArrayM12 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 =
  js_buildArrayM12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12
{-# INLINE buildArrayM12 #-}

js_buildArrayM12 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM12 = stub


buildArrayM13 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 =
  js_buildArrayM13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
{-# INLINE buildArrayM13 #-}

js_buildArrayM13 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM13 = stub


buildArrayM14 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 =
  js_buildArrayM14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
{-# INLINE buildArrayM14 #-}

js_buildArrayM14 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM14 = stub


buildArrayM15 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 =
  js_buildArrayM15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
{-# INLINE buildArrayM15 #-}

js_buildArrayM15 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM15 = stub


buildArrayM16 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM16 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 =
  js_buildArrayM16 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16
{-# INLINE buildArrayM16 #-}

js_buildArrayM16 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM16 = stub


buildArrayM17 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM17 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 =
  js_buildArrayM17 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17
{-# INLINE buildArrayM17 #-}

js_buildArrayM17 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM17 = stub


buildArrayM18 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM18 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 =
  js_buildArrayM18 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18
{-# INLINE buildArrayM18 #-}

js_buildArrayM18 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM18 = stub

buildArrayM19 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM19 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 =
  js_buildArrayM19 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19
{-# INLINE buildArrayM19 #-}

js_buildArrayM19 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM19 = stub


buildArrayM20 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM20 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 =
  js_buildArrayM20 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
{-# INLINE buildArrayM20 #-}

js_buildArrayM20 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM20 = stub

buildArrayM21 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM21 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 =
  js_buildArrayM21 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21
{-# INLINE buildArrayM21 #-}

js_buildArrayM21 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM21 = stub

buildArrayM22 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM22 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 =
  js_buildArrayM22 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22
{-# INLINE buildArrayM22 #-}

js_buildArrayM22 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM22 = stub

buildArrayM23 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM23 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 =
  js_buildArrayM23 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23
{-# INLINE buildArrayM23 #-}

js_buildArrayM23 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM23 = stub

buildArrayM24 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM24 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 =
  js_buildArrayM24 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24
{-# INLINE buildArrayM24 #-}

js_buildArrayM24 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM24 = stub

buildArrayM25 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM25 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 =
  js_buildArrayM25 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25
{-# INLINE buildArrayM25 #-}

js_buildArrayM25 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM25 = stub

buildArrayM26 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM26 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 =
  js_buildArrayM26 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26
{-# INLINE buildArrayM26 #-}

js_buildArrayM26 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM26 = stub

buildArrayM27 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM27 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 =
  js_buildArrayM27 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27
{-# INLINE buildArrayM27 #-}

js_buildArrayM27 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM27 = stub

buildArrayM28 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM28 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 =
  js_buildArrayM28 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28
{-# INLINE buildArrayM28 #-}

js_buildArrayM28 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM28 = stub

buildArrayM29 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM29 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 =
  js_buildArrayM29 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29
{-# INLINE buildArrayM29 #-}

js_buildArrayM29 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM29 = stub

buildArrayM30 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM30 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 =
  js_buildArrayM30 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
{-# INLINE buildArrayM30 #-}

js_buildArrayM30 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM30 = stub

buildArrayM31 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM31 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 =
  js_buildArrayM31 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
{-# INLINE buildArrayM31 #-}

js_buildArrayM31 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM31 = stub

buildArrayM32 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
buildArrayM32 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 =
  js_buildArrayM32 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32
{-# INLINE buildArrayM32 #-}

js_buildArrayM32 :: J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> J -> IO A
js_buildArrayM32 = stub

js_buildObjectFromTupListI :: Any -> O
js_buildObjectFromTupListI = stub

js_emptyObjectI :: O
js_emptyObjectI = stub

buildObjectI :: [(K,V)] -> O
buildObjectI xs = js_buildObjectFromTupListI . unsafeCoerce . seqTupList $ xs
{-# INLINE [1] buildObjectI #-}
{-# RULES "buildObjectI/empty" buildObjectI [] = js_emptyObjectI #-}
{-# RULES "buildObjectI/buildObjectI1" forall k1 v1. buildObjectI [(k1,v1)] = buildObjectI1 k1 v1 #-}
{-# RULES "buildObjectI/buildObjectI2" forall k1 v1 k2 v2. buildObjectI [(k1,v1),(k2,v2)] = buildObjectI2 k1 v1 k2 v2 #-}
{-# RULES "buildObjectI/buildObjectI3" forall k1 v1 k2 v2 k3 v3. buildObjectI [(k1,v1),(k2,v2),(k3,v3)] = buildObjectI3 k1 v1 k2 v2 k3 v3 #-}
{-# RULES "buildObjectI/buildObjectI4" forall k1 v1 k2 v2 k3 v3 k4 v4. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4)] = buildObjectI4 k1 v1 k2 v2 k3 v3 k4 v4 #-}
{-# RULES "buildObjectI/buildObjectI5" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5)] = buildObjectI5 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 #-}
{-# RULES "buildObjectI/buildObjectI6" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6)] = buildObjectI6 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 #-}
{-# RULES "buildObjectI/buildObjectI7" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7)] = buildObjectI7 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 #-}
{-# RULES "buildObjectI/buildObjectI8" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8)] = buildObjectI8 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 #-}
{-# RULES "buildObjectI/buildObjectI9" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9)] = buildObjectI9 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 #-}
{-# RULES "buildObjectI/buildObjectI10" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10)] = buildObjectI10 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 #-}
{-# RULES "buildObjectI/buildObjectI11" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11)] = buildObjectI11 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 #-}
{-# RULES "buildObjectI/buildObjectI12" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12)] = buildObjectI12 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 #-}
{-# RULES "buildObjectI/buildObjectI13" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13)] = buildObjectI13 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 #-}
{-# RULES "buildObjectI/buildObjectI14" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14)] = buildObjectI14 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 #-}
{-# RULES "buildObjectI/buildObjectI15" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15)] = buildObjectI15 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 #-}
{-# RULES "buildObjectI/buildObjectI16" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16)] = buildObjectI16 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 #-}
{-# RULES "buildObjectI/buildObjectI17" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17)] = buildObjectI17 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 #-}
{-# RULES "buildObjectI/buildObjectI18" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18)] = buildObjectI18 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 #-}
{-# RULES "buildObjectI/buildObjectI19" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19)] = buildObjectI19 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 #-}
{-# RULES "buildObjectI/buildObjectI20" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20)] = buildObjectI20 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 #-}
{-# RULES "buildObjectI/buildObjectI21" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21)] = buildObjectI21 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 #-}
{-# RULES "buildObjectI/buildObjectI22" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22)] = buildObjectI22 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 #-}
{-# RULES "buildObjectI/buildObjectI23" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23)] = buildObjectI23 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 #-}
{-# RULES "buildObjectI/buildObjectI24" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24)] = buildObjectI24 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 #-}
{-# RULES "buildObjectI/buildObjectI25" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25)] = buildObjectI25 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 #-}
{-# RULES "buildObjectI/buildObjectI26" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25),(k26,v26)] = buildObjectI26 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 #-}
{-# RULES "buildObjectI/buildObjectI27" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25),(k26,v26),(k27,v27)] = buildObjectI27 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 #-}
{-# RULES "buildObjectI/buildObjectI28" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25),(k26,v26),(k27,v27),(k28,v28)] = buildObjectI28 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 #-}
{-# RULES "buildObjectI/buildObjectI29" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25),(k26,v26),(k27,v27),(k28,v28),(k29,v29)] = buildObjectI29 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 #-}
{-# RULES "buildObjectI/buildObjectI30" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25),(k26,v26),(k27,v27),(k28,v28),(k29,v29),(k30,v30)] = buildObjectI30 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 #-}
{-# RULES "buildObjectI/buildObjectI31" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25),(k26,v26),(k27,v27),(k28,v28),(k29,v29),(k30,v30),(k31,v31)] = buildObjectI31 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31 #-}
{-# RULES "buildObjectI/buildObjectI32" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31 k32 v32. buildObjectI [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25),(k26,v26),(k27,v27),(k28,v28),(k29,v29),(k30,v30),(k31,v31),(k32,v32)] = buildObjectI32 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31 k32 v32 #-}

buildObjectI1 :: K -> V -> O
buildObjectI1 k1 v1 =
  js_buildObjectI1 k1 v1
{-# INLINE buildObjectI1 #-}

js_buildObjectI1 :: K -> V -> O
js_buildObjectI1 = stub


buildObjectI2 :: K -> V -> K -> V -> O
buildObjectI2 k1 v1 k2 v2 =
  js_buildObjectI2 k1 v1 k2 v2
{-# INLINE buildObjectI2 #-}

js_buildObjectI2 :: K -> V -> K -> V -> O
js_buildObjectI2 = stub


buildObjectI3 :: K -> V -> K -> V -> K -> V -> O
buildObjectI3 k1 v1 k2 v2 k3 v3 =
  js_buildObjectI3 k1 v1 k2 v2 k3 v3
{-# INLINE buildObjectI3 #-}

js_buildObjectI3 :: K -> V -> K -> V -> K -> V -> O
js_buildObjectI3 = stub


buildObjectI4 :: K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI4 k1 v1 k2 v2 k3 v3 k4 v4 =
  js_buildObjectI4 k1 v1 k2 v2 k3 v3 k4 v4
{-# INLINE buildObjectI4 #-}

js_buildObjectI4 :: K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI4 = stub


buildObjectI5 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI5 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 =
  js_buildObjectI5 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5
{-# INLINE buildObjectI5 #-}

js_buildObjectI5 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI5 = stub


buildObjectI6 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI6 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 =
  js_buildObjectI6 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6
{-# INLINE buildObjectI6 #-}

js_buildObjectI6 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI6 = stub


buildObjectI7 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI7 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 =
  js_buildObjectI7 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7
{-# INLINE buildObjectI7 #-}

js_buildObjectI7 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI7 = stub


buildObjectI8 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI8 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 =
  js_buildObjectI8 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8
{-# INLINE buildObjectI8 #-}

js_buildObjectI8 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI8 = stub


buildObjectI9 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI9 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 =
  js_buildObjectI9 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9
{-# INLINE buildObjectI9 #-}

js_buildObjectI9 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI9 = stub


buildObjectI10 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI10 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 =
  js_buildObjectI10 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10
{-# INLINE buildObjectI10 #-}

js_buildObjectI10 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI10 = stub

buildObjectI11 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI11 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 =
  js_buildObjectI11 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11
{-# INLINE buildObjectI11 #-}

js_buildObjectI11 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI11 = stub

buildObjectI12 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI12 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 =
  js_buildObjectI12 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12
{-# INLINE buildObjectI12 #-}

js_buildObjectI12 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI12 = stub

buildObjectI13 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI13 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 =
  js_buildObjectI13 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13
{-# INLINE buildObjectI13 #-}

js_buildObjectI13 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI13 = stub

buildObjectI14 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI14 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 =
  js_buildObjectI14 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14
{-# INLINE buildObjectI14 #-}

js_buildObjectI14 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI14 = stub

buildObjectI15 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI15 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 =
  js_buildObjectI15 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15
{-# INLINE buildObjectI15 #-}

js_buildObjectI15 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI15 = stub

buildObjectI16 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI16 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 =
  js_buildObjectI16 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16
{-# INLINE buildObjectI16 #-}

js_buildObjectI16 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI16 = stub

buildObjectI17 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI17 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 =
  js_buildObjectI17 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17
{-# INLINE buildObjectI17 #-}

js_buildObjectI17 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI17 = stub

buildObjectI18 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI18 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 =
  js_buildObjectI18 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18
{-# INLINE buildObjectI18 #-}

js_buildObjectI18 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI18 = stub

buildObjectI19 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI19 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 =
  js_buildObjectI19 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19
{-# INLINE buildObjectI19 #-}

js_buildObjectI19 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI19 = stub

buildObjectI20 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI20 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 =
  js_buildObjectI20 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20
{-# INLINE buildObjectI20 #-}

js_buildObjectI20 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI20 = stub

buildObjectI21 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI21 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 =
  js_buildObjectI21 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21
{-# INLINE buildObjectI21 #-}

js_buildObjectI21 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI21 = stub

buildObjectI22 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI22 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 =
  js_buildObjectI22 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22
{-# INLINE buildObjectI22 #-}

js_buildObjectI22 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI22 = stub

buildObjectI23 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI23 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 =
  js_buildObjectI23 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23
{-# INLINE buildObjectI23 #-}

js_buildObjectI23 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI23 = stub

buildObjectI24 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI24 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 =
  js_buildObjectI24 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24
{-# INLINE buildObjectI24 #-}

js_buildObjectI24 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI24 = stub

buildObjectI25 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI25 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 =
  js_buildObjectI25 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25
{-# INLINE buildObjectI25 #-}

js_buildObjectI25 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI25 = stub

buildObjectI26 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI26 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 =
  js_buildObjectI26 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26
{-# INLINE buildObjectI26 #-}

js_buildObjectI26 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI26 = stub

buildObjectI27 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI27 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 =
  js_buildObjectI27 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27
{-# INLINE buildObjectI27 #-}

js_buildObjectI27 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI27 = stub

buildObjectI28 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI28 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 =
  js_buildObjectI28 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28
{-# INLINE buildObjectI28 #-}

js_buildObjectI28 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI28 = stub

buildObjectI29 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI29 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 =
  js_buildObjectI29 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29
{-# INLINE buildObjectI29 #-}

js_buildObjectI29 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI29 = stub

buildObjectI30 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI30 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 =
  js_buildObjectI30 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30
{-# INLINE buildObjectI30 #-}

js_buildObjectI30 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI30 = stub

buildObjectI31 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI31 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31 =
  js_buildObjectI31 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31
{-# INLINE buildObjectI31 #-}

js_buildObjectI31 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI31 = stub

buildObjectI32 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
buildObjectI32 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31 k32 v32 =
  js_buildObjectI32 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31 k32 v32
{-# INLINE buildObjectI32 #-}

js_buildObjectI32 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> O
js_buildObjectI32 = stub

js_buildObjectFromTupListM :: Any -> IO O
js_buildObjectFromTupListM = stub

js_emptyObjectM :: IO O
js_emptyObjectM = stub

buildObjectM :: [(K,V)] -> IO O
buildObjectM xs = js_buildObjectFromTupListM . unsafeCoerce . seqTupList $ xs
{-# INLINE [1] buildObjectM #-}
{-# RULES "buildObjectM/empty" buildObjectM [] = js_emptyObjectM #-}
{-# RULES "buildObjectM/buildObjectM1" forall k1 v1. buildObjectM [(k1,v1)] = buildObjectM1 k1 v1 #-}
{-# RULES "buildObjectM/buildObjectM2" forall k1 v1 k2 v2. buildObjectM [(k1,v1),(k2,v2)] = buildObjectM2 k1 v1 k2 v2 #-}
{-# RULES "buildObjectM/buildObjectM3" forall k1 v1 k2 v2 k3 v3. buildObjectM [(k1,v1),(k2,v2),(k3,v3)] = buildObjectM3 k1 v1 k2 v2 k3 v3 #-}
{-# RULES "buildObjectM/buildObjectM4" forall k1 v1 k2 v2 k3 v3 k4 v4. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4)] = buildObjectM4 k1 v1 k2 v2 k3 v3 k4 v4 #-}
{-# RULES "buildObjectM/buildObjectM5" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5)] = buildObjectM5 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 #-}
{-# RULES "buildObjectM/buildObjectM6" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6)] = buildObjectM6 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 #-}
{-# RULES "buildObjectM/buildObjectM7" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7)] = buildObjectM7 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 #-}
{-# RULES "buildObjectM/buildObjectM8" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8)] = buildObjectM8 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 #-}
{-# RULES "buildObjectM/buildObjectM9" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9)] = buildObjectM9 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 #-}
{-# RULES "buildObjectM/buildObjectM10" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10)] = buildObjectM10 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 #-}
{-# RULES "buildObjectM/buildObjectM11" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11)] = buildObjectM11 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 #-}
{-# RULES "buildObjectM/buildObjectM12" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12)] = buildObjectM12 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 #-}
{-# RULES "buildObjectM/buildObjectM13" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13)] = buildObjectM13 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 #-}
{-# RULES "buildObjectM/buildObjectM14" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14)] = buildObjectM14 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 #-}
{-# RULES "buildObjectM/buildObjectM15" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15)] = buildObjectM15 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 #-}
{-# RULES "buildObjectM/buildObjectM16" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16)] = buildObjectM16 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 #-}
{-# RULES "buildObjectM/buildObjectM17" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17)] = buildObjectM17 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 #-}
{-# RULES "buildObjectM/buildObjectM18" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18)] = buildObjectM18 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 #-}
{-# RULES "buildObjectM/buildObjectM19" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19)] = buildObjectM19 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 #-}
{-# RULES "buildObjectM/buildObjectM20" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20)] = buildObjectM20 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 #-}
{-# RULES "buildObjectM/buildObjectM21" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21)] = buildObjectM21 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 #-}
{-# RULES "buildObjectM/buildObjectM22" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22)] = buildObjectM22 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 #-}
{-# RULES "buildObjectM/buildObjectM23" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23)] = buildObjectM23 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 #-}
{-# RULES "buildObjectM/buildObjectM24" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24)] = buildObjectM24 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 #-}
{-# RULES "buildObjectM/buildObjectM25" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25)] = buildObjectM25 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 #-}
{-# RULES "buildObjectM/buildObjectM26" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25),(k26,v26)] = buildObjectM26 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 #-}
{-# RULES "buildObjectM/buildObjectM27" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25),(k26,v26),(k27,v27)] = buildObjectM27 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 #-}
{-# RULES "buildObjectM/buildObjectM28" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25),(k26,v26),(k27,v27),(k28,v28)] = buildObjectM28 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 #-}
{-# RULES "buildObjectM/buildObjectM29" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25),(k26,v26),(k27,v27),(k28,v28),(k29,v29)] = buildObjectM29 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 #-}
{-# RULES "buildObjectM/buildObjectM30" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25),(k26,v26),(k27,v27),(k28,v28),(k29,v29),(k30,v30)] = buildObjectM30 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 #-}
{-# RULES "buildObjectM/buildObjectM31" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25),(k26,v26),(k27,v27),(k28,v28),(k29,v29),(k30,v30),(k31,v31)] = buildObjectM31 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31 #-}
{-# RULES "buildObjectM/buildObjectM32" forall k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31 k32 v32. buildObjectM [(k1,v1),(k2,v2),(k3,v3),(k4,v4),(k5,v5),(k6,v6),(k7,v7),(k8,v8),(k9,v9),(k10,v10),(k11,v11),(k12,v12),(k13,v13),(k14,v14),(k15,v15),(k16,v16),(k17,v17),(k18,v18),(k19,v19),(k20,v20),(k21,v21),(k22,v22),(k23,v23),(k24,v24),(k25,v25),(k26,v26),(k27,v27),(k28,v28),(k29,v29),(k30,v30),(k31,v31),(k32,v32)] = buildObjectM32 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31 k32 v32 #-}

buildObjectM1 :: K -> V -> IO O
buildObjectM1 k1 v1 =
  js_buildObjectM1 k1 v1
{-# INLINE buildObjectM1 #-}

js_buildObjectM1 :: K -> V -> IO O
js_buildObjectM1 = stub


buildObjectM2 :: K -> V -> K -> V -> IO O
buildObjectM2 k1 v1 k2 v2 =
  js_buildObjectM2 k1 v1 k2 v2
{-# INLINE buildObjectM2 #-}

js_buildObjectM2 :: K -> V -> K -> V -> IO O
js_buildObjectM2 = stub


buildObjectM3 :: K -> V -> K -> V -> K -> V -> IO O
buildObjectM3 k1 v1 k2 v2 k3 v3 =
  js_buildObjectM3 k1 v1 k2 v2 k3 v3
{-# INLINE buildObjectM3 #-}

js_buildObjectM3 :: K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM3 = stub


buildObjectM4 :: K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM4 k1 v1 k2 v2 k3 v3 k4 v4 =
  js_buildObjectM4 k1 v1 k2 v2 k3 v3 k4 v4
{-# INLINE buildObjectM4 #-}

js_buildObjectM4 :: K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM4 = stub


buildObjectM5 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM5 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 =
  js_buildObjectM5 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5
{-# INLINE buildObjectM5 #-}

js_buildObjectM5 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM5 = stub


buildObjectM6 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM6 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 =
  js_buildObjectM6 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6
{-# INLINE buildObjectM6 #-}

js_buildObjectM6 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM6 = stub


buildObjectM7 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM7 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 =
  js_buildObjectM7 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7
{-# INLINE buildObjectM7 #-}

js_buildObjectM7 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM7 = stub


buildObjectM8 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM8 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 =
  js_buildObjectM8 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8
{-# INLINE buildObjectM8 #-}

js_buildObjectM8 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM8 = stub


buildObjectM9 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM9 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 =
  js_buildObjectM9 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9
{-# INLINE buildObjectM9 #-}

js_buildObjectM9 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM9 = stub


buildObjectM10 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM10 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 =
  js_buildObjectM10 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10
{-# INLINE buildObjectM10 #-}

js_buildObjectM10 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM10 = stub

buildObjectM11 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM11 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 =
  js_buildObjectM11 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11
{-# INLINE buildObjectM11 #-}

js_buildObjectM11 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM11 = stub

buildObjectM12 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM12 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 =
  js_buildObjectM12 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12
{-# INLINE buildObjectM12 #-}

js_buildObjectM12 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM12 = stub

buildObjectM13 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM13 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 =
  js_buildObjectM13 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13
{-# INLINE buildObjectM13 #-}

js_buildObjectM13 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM13 = stub

buildObjectM14 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM14 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 =
  js_buildObjectM14 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14
{-# INLINE buildObjectM14 #-}

js_buildObjectM14 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM14 = stub

buildObjectM15 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM15 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 =
  js_buildObjectM15 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15
{-# INLINE buildObjectM15 #-}

js_buildObjectM15 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM15 = stub

buildObjectM16 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM16 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 =
  js_buildObjectM16 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16
{-# INLINE buildObjectM16 #-}

js_buildObjectM16 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM16 = stub

buildObjectM17 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM17 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 =
  js_buildObjectM17 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17
{-# INLINE buildObjectM17 #-}

js_buildObjectM17 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM17 = stub

buildObjectM18 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM18 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 =
  js_buildObjectM18 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18
{-# INLINE buildObjectM18 #-}

js_buildObjectM18 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM18 = stub

buildObjectM19 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM19 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 =
  js_buildObjectM19 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19
{-# INLINE buildObjectM19 #-}

js_buildObjectM19 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM19 = stub

buildObjectM20 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM20 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 =
  js_buildObjectM20 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20
{-# INLINE buildObjectM20 #-}

js_buildObjectM20 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM20 = stub

buildObjectM21 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM21 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 =
  js_buildObjectM21 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21
{-# INLINE buildObjectM21 #-}

js_buildObjectM21 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM21 = stub

buildObjectM22 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM22 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 =
  js_buildObjectM22 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22
{-# INLINE buildObjectM22 #-}

js_buildObjectM22 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM22 = stub

buildObjectM23 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM23 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 =
  js_buildObjectM23 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23
{-# INLINE buildObjectM23 #-}

js_buildObjectM23 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM23 = stub

buildObjectM24 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM24 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 =
  js_buildObjectM24 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24
{-# INLINE buildObjectM24 #-}

js_buildObjectM24 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM24 = stub

buildObjectM25 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM25 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 =
  js_buildObjectM25 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25
{-# INLINE buildObjectM25 #-}

js_buildObjectM25 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM25 = stub

buildObjectM26 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM26 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 =
  js_buildObjectM26 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26
{-# INLINE buildObjectM26 #-}

js_buildObjectM26 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM26 = stub

buildObjectM27 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM27 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 =
  js_buildObjectM27 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27
{-# INLINE buildObjectM27 #-}

js_buildObjectM27 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM27 = stub

buildObjectM28 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM28 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 =
  js_buildObjectM28 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28
{-# INLINE buildObjectM28 #-}

js_buildObjectM28 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM28 = stub

buildObjectM29 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM29 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 =
  js_buildObjectM29 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29
{-# INLINE buildObjectM29 #-}

js_buildObjectM29 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM29 = stub

buildObjectM30 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM30 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 =
  js_buildObjectM30 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30
{-# INLINE buildObjectM30 #-}

js_buildObjectM30 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O

js_buildObjectM30 = stub

buildObjectM31 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM31 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31 =
  js_buildObjectM31 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31
{-# INLINE buildObjectM31 #-}

js_buildObjectM31 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O

js_buildObjectM31 = stub

buildObjectM32 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
buildObjectM32 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31 k32 v32 =
  js_buildObjectM32 k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 k27 v27 k28 v28 k29 v29 k30 v30 k31 v31 k32 v32
{-# INLINE buildObjectM32 #-}


js_buildObjectM32 :: K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> K -> V -> IO O
js_buildObjectM32 = stub





