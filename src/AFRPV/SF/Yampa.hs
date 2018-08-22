-----------------------------------------------------------------------------
-- |
-- Module      :  AFRPV.SF.Yampa
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Yampa specific arrow network primitives.
--
-----------------------------------------------------------------------------

  {-# LANGUAGE

      LambdaCase
    ,  GADTs
    , Rank2Types

    #-}

-----------------------------------------------------------------------------

module AFRPV.SF.Yampa
  ( WSF
  , Delayed
  , Network
  , YampaPrimitive(..)
  , components
  ) where

-----------------------------------------------------------------------------

import AFRPV.SF.Observer
  ( ArrNet(..)
  , Observe(..)
  , netComponents
  , netCollect
  )

import FRP.Yampa
  ( SF
  )

-----------------------------------------------------------------------------

-- | Simple flag to distinguish similar delayed from non-delayed
-- operations.

type Delayed = Bool

-----------------------------------------------------------------------------

-- | Network type combining the basic arrow operations with the Yampa
-- specific ones.

type Network a = ArrNet (YampaPrimitive a)

-----------------------------------------------------------------------------

-- | Yampa specific network primitives. This selection alread
-- represents a simplification against the full number of Yampa
-- specific primitives. Respecting a common type signature multiple
-- Yampa primitives are grouped hiding specifics that are not
-- important for visualization later. However the parameterized type
-- can be used to store such information if needed.

data YampaPrimitive a =
    -- | pure function application except for the first invocation
    Initially
    -- | source components (type: 'SF a b')
  | Source a
    -- | delay components (type: 'SF (Event a) (Event a)')
  | Delay a
    -- | trigger components (type: 'SF a (Event b)')
  | Trigger a
    -- | hold components (tuyp: 'SF (Event a) b')
  | Hold a
    -- | filter components (type: 'SF (Event a) (Event a)')
  | Filter a
    -- | stateful components (type 'SF a b')
  | Accum a
    -- | variable delay
  | Pause (Network a) (Network a)
    -- | switches
  | Switch Delayed (Network a)
  | RSwitch Delayed (Network a)
  | KSwitch Delayed (Network a) (Network a)
  | forall col. Functor col => Par (col (Network a))
  | forall col. Functor col => PSwitch Delayed (col (Network a)) (Network a)
  | forall col. Functor col => RPSwitch Delayed (col (Network a))

-----------------------------------------------------------------------------

-- | Extended wrapper signal function type.

type WSF = Observe (YampaPrimitive String) SF

-----------------------------------------------------------------------------

-- | Extracts the list of all individual components from the tree
-- based network structure.

components
  :: Network a -> [Network a]

components =
  netComponents collect

  where
    collect
      :: [Network a] -> YampaPrimitive a -> [Network a]

    collect a c = (Other c :) $ case c of
      Initially     -> a
      Source _      -> a
      Delay _       -> a
      Trigger _     -> a
      Hold _        -> a
      Filter _      -> a
      Accum _       -> a
      Pause x y     -> netCollect collect (netCollect collect a x) y
      Switch _ x    -> netCollect collect a x
      RSwitch _ x   -> netCollect collect a x
      KSwitch _ x y -> netCollect collect (netCollect collect a x) y
      _             ->
        error "NOT IMPLEMENTED: Collections are not supported yet"

-----------------------------------------------------------------------------
