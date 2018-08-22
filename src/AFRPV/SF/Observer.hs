-----------------------------------------------------------------------------
-- |
-- Module      :  AFRPV.SF.Observer
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- The observer arrow wrapper encapsulates the FRP arrow of a given
-- arrow based FRP library, while at the same recording the underlying
-- structure during the arrow creation.
--
-----------------------------------------------------------------------------

  {-# LANGUAGE

      LambdaCase

    #-}

-----------------------------------------------------------------------------

module AFRPV.SF.Observer
  ( ArrNet(..)
  , Observe(..)
  , netComponents
  , netCollect
  ) where

-----------------------------------------------------------------------------

import Prelude hiding
  ( (.)
  , id
  )

import Control.Arrow
  ( Arrow(..)
  , ArrowChoice(..)
  , ArrowLoop(..)
  )

import Control.Category
  ( Category(..)
  )

-----------------------------------------------------------------------------

-- | Structure of the arrow network using the class based arrow
-- operations. Additionally, the 'Other' primitive can be used in
-- combination with the parameterized type to add library specific
-- instances, such as switches, hold, or delay operations.

data ArrNet s =
    Id                                 -- ^ Category:     id
  | Pure                               -- ^ Arrow:        arr
  | First (ArrNet s)                   -- ^ Arrow:        first
  | Second (ArrNet s)                  -- ^ Arrow:        second
  | Compose (ArrNet s) (ArrNet s)      -- ^ Category:     (.)
  | Parallel (ArrNet s) (ArrNet s)     -- ^ Arrow:        (***)
  | FanOut (ArrNet s) (ArrNet s)       -- ^ Arrow:        (&&&)
  | CLeft (ArrNet s)                   -- ^ ArrowChoice:  left
  | CRight (ArrNet s)                  -- ^ ArrowChoice:  right
  | Merge (ArrNet s) (ArrNet s)        -- ^ ArrowChoice:  (+++)
  | FanIn (ArrNet s) (ArrNet s)        -- ^ ArrowChoice:  (|||)
  | Loop (ArrNet s)                    -- ^ ArrowLoop:    loop
  | Other s                            -- ^ library specific primitives

-----------------------------------------------------------------------------

-- | Wrapper structure containing the wrapped arrow as well as the
-- respective arrow network.

data Observe s a b c =
  OA
    { arrow :: a b c
    , net :: ArrNet s
    }

-----------------------------------------------------------------------------

-- | The 'Category' instance is supported via 'Id' and 'Compose'.

instance Category a => Category (Observe s a) where
  id = OA id Id
  OA f x . OA g y = OA (f . g) (Compose x y)

-----------------------------------------------------------------------------

-- | The 'Arrow' instance is supported via 'Pure', 'First', 'Second',
-- 'Parallel', and 'Fanout'.

instance Arrow a => Arrow (Observe s a) where
  arr f = OA (arr f) Pure
  first (OA a x) = OA (first a) (First x)
  second (OA a x) = OA (second a) (Second x)
  OA a x *** OA b y = OA (a *** b) (Parallel x y)
  OA a x &&& OA b y = OA (a &&& b) (FanOut x y)

-----------------------------------------------------------------------------

-- | The 'ArrowChoice' instance is supported via 'CLeft', 'CRight',
-- 'Merge', and 'FanIn'.

instance ArrowChoice a => ArrowChoice (Observe s a) where
  left (OA a x) = OA (left a) (CLeft x)
  right (OA a x) = OA (right a) (CRight x)
  OA a x +++ OA b y = OA (a +++ b) (Merge x y)
  OA a x ||| OA b y = OA (a ||| b) (FanIn x y)

-----------------------------------------------------------------------------

-- | The 'ArrowLoop' instance is supported via 'Loop'.

instance ArrowLoop a => ArrowLoop (Observe s a ) where
  loop (OA a x) = OA (loop a) (Loop x)

-----------------------------------------------------------------------------

-- | Utility function to extract a list of all components of the
-- network. The first argument is required to provide a respective
-- extractor for the library specific components.

netComponents
  :: ([ArrNet s] -> s -> [ArrNet s]) -> ArrNet s -> [ArrNet s]

netComponents e = reverse . netCollect e []

-----------------------------------------------------------------------------

-- | Internal collector of 'netComponents' with an explicit
-- accumulator.

netCollect
  :: ([ArrNet s] -> s -> [ArrNet s]) -> [ArrNet s] -> ArrNet s -> [ArrNet s]

netCollect e a c = case c of
  Id           -> c : a
  Pure         -> c : a
  First x      -> c : netCollect e a x
  Second x     -> c : netCollect e a x
  Compose x y  -> c : netCollect e (netCollect e a x) y
  Parallel x y -> c : netCollect e (netCollect e a x) y
  FanOut x y   -> c : netCollect e (netCollect e a x) y
  CLeft x      -> c : netCollect e a x
  CRight x     -> c : netCollect e a x
  Merge x y    -> c : netCollect e (netCollect e a x) y
  FanIn x y    -> c : netCollect e (netCollect e a x) y
  Loop x       -> c : netCollect e a x
  Other x      -> e a x

-----------------------------------------------------------------------------
