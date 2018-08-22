-----------------------------------------------------------------------------
-- |
-- Module      :  AFRPV.Yampa
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- This is a wrapper module for the @FRP.Yampa@ module (v0.11).  The
-- wrapper extends the Yampa signal function such that it also records
-- the created arrow network structure. An extend signal function
-- definition is provided that serves exactely the same capabilities
-- as the original signal function, except that the created network
-- structure can also be exported and post-processed after the arrow
-- creation. The new signal arrow can also be reduced to the original
-- signal arrow using the 'original' function. The network can be
-- visualized using the 'renderNetwork' function.
--
-- Internally, the module also re-exports all methods of @FRP.Yampa@,
-- where functions are adapted to the extend signal function
-- definition where necesscary. By using proper renaming this module
-- provides the exact same interface as @FRP.Yampa@ and can simply be
-- used, even in already existing projects, by just replacing the
-- import of @FRP.Yampa@ with @AFRPV.Yampa@.
--
-- Using specific methods from FRP.Yampa.(...) submodules is currently
-- not supported.
--
-- All documentation below this line is inherited from the original
-- @FRP.Yampa@ module (v0.11).
--
-- \-----------------------
--
-- Domain-specific language embedded in Haskell for programming hybrid
-- (mixed discrete-time and continuous-time) systems. Yampa is based
-- on the concepts of Functional Reactive Programming (FRP) and is
-- structured using arrow combinators.
--
-- You can find examples, screenshots, tutorials and documentation
-- here:
--
-- <https://github.com/ivanperez-keera/Yampa>
--
-- <https://github.com/ivanperez-keera/Yampa/tree/master/examples>
--
-- <https://wiki.haskell.org/Yampa>
--
--
-- Structuring a hybrid system in Yampa is done based on two main
-- concepts:
--
-- * Signal Functions: 'SF'. Yampa is based on the concept of Signal
-- Functions, which are functions from a typed input signal to a typed
-- output signal.  Conceptually, signals are functions from Time to
-- Value, where time are the real numbers and, computationally, a very
-- dense approximation (Double) is used.
--
-- * Events: 'Event'. Values that may or may not occur (and would
-- probably occur rarely). It is often used for incoming network
-- messages, mouse clicks, etc. Events are used as values carried by
-- signals.
--
-- A complete Yampa system is defined as one Signal Function from some
-- type @a@ to a type @b@. The execution of this signal transformer
-- with specific input can be accomplished by means of two functions:
-- 'reactimate' (which needs an initialization action, an input
-- sensing action and an actuation/consumer action and executes until
-- explicitly stopped), and 'react' (which executes only one cycle).
--
--
-- Main Yampa modules:
--
-- * "FRP.Yampa"            -- This exports all FRP-related functions
--
-- * "FRP.Yampa.Task"
--
-- Minimal Complete FRP Definition:
--
-- * "FRP.Yampa.Core"
--
-- Different FRP aspects:
--
-- * "FRP.Yampa.Basic"
--
-- * "FRP.Yampa.Conditional"
--
-- * "FRP.Yampa.Delays"
--
-- * "FRP.Yampa.Event"
--
-- * "FRP.Yampa.EventS"       -- Event consuming/producing SFs. To be renamed.
--
-- * "FRP.Yampa.Hybrid"       -- Hybrid (discrete/continuous) SFs
--
-- * "FRP.Yampa.Integration"
--
-- * "FRP.Yampa.Loop"
--
-- * "FRP.Yampa.Random"
--
-- * "FRP.Yampa.Scan"
--
-- * "FRP.Yampa.Switches"
--
-- * "FRP.Yampa.Time"
--
-- * "FRP.Yampa.Simulation" -- Reactimation/evaluation
--
-- Internals:
--
-- * "FRP.Yampa.InternalCore" -- Module not exposed.
--
-- Geometry:
--
-- * "FRP.Yampa.Geometry"
--
-- * "FRP.Yampa.AffineSpace"
--
-- * "FRP.Yampa.Vector Space"
--
-- * "FRP.Yampa.Point2"
--
-- * "FRP.Yampa.Point3"
--
-- * "FRP.Yampa.Vector2"
--
-- * "FRP.Yampa.Vector3"
--
-- Old legacy code:
--
-- * "FRP.Yampa.Diagnostics"
--
-- * "FRP.Yampa.Forceable"
--
-- * "FRP.Yampa.Internals"  -- No longer in use
--
-- * "FRP.Yampa.MergeableRecord"
--
-- * "FRP.Yampa.Miscellany"
--
-- * "FRP.Yampa.Utilities"
--
-- This will be the last version of Yampa to include mergeable
-- records, point2 and point3, vector2 and vector3, and other
-- auxiliary definitions. The internals have now changed. Also, please
-- let us know if you see any problems with the new project structure.

-----------------------------------------------------------------------------

  {-# LANGUAGE

      GADTs
    , Rank2Types

    #-}

-----------------------------------------------------------------------------

module AFRPV.Yampa
  ( module Control.Arrow
  , module FRP.Yampa.VectorSpace
  , RandomGen(..)
  , Random(..)

  -- * Basic definitions
  , Time
  , DTime
  , SF
  , Event(..)

  -- ** Lifting
  , arrPrim
  , arrEPrim

  -- * Signal functions

  -- ** Basic signal functions
  , identity
  , constant
  , localTime
  , time

  -- ** Initialization
  , (-->)
  , (-:>)
  , (>--)
  , (-=>)
  , (>=-)
  , initially

  -- ** Simple, stateful signal processing
  , sscan
  , sscanPrim

  -- * Events

  -- ** Basic event sources
  , never
  , now
  , after
  , repeatedly
  , afterEach
  , afterEachCat
  , delayEvent
  , delayEventCat
  , edge
  , iEdge
  , edgeTag
  , edgeJust
  , edgeBy
  , maybeToEvent

  -- ** Stateful event suppression
  , notYet
  , once
  , takeEvents
  , dropEvents

  -- ** Pointwise functions on events
  , noEvent
  , noEventFst
  , noEventSnd
  , event
  , fromEvent
  , isEvent
  , isNoEvent
  , tag
  , tagWith
  , attach
  , lMerge
  , rMerge
  , merge
  , mergeBy
  , mapMerge
  , mergeEvents
  , catEvents
  , joinE
  , splitE
  , filterE
  , mapFilterE
  , gate

  -- * Switching

  -- ** Basic switchers
  , switch
  , dSwitch
  , rSwitch
  , drSwitch
  , kSwitch
  , dkSwitch

  -- ** Parallel composition and switching

  -- *** Parallel composition and switching over collections with broadcasting
  , parB
  , pSwitchB
  , dpSwitchB
  , rpSwitchB
  , drpSwitchB

  -- *** Parallel composition and switching over collections with general routing
  , par
  , pSwitch
  , dpSwitch
  , rpSwitch
  , drpSwitch

  -- * Discrete to continuous-time signal functions

  -- ** Wave-form generation
  , hold
  , dHold
  , trackAndHold

  -- ** Accumulators
  , accum
  , accumHold
  , dAccumHold
  , accumBy
  , accumHoldBy
  , dAccumHoldBy
  , accumFilter

  -- * Delays

  -- ** Basic delays
  , pre
  , iPre

  -- ** Timed delays
  , delay

  -- ** Variable delay
  , pause

  -- * State keeping combinators

  -- ** Loops with guaranteed well-defined feedback
  , loopPre
  , loopIntegral

  -- ** Integration and differentiation
  , integral
  , imIntegral
  , impulseIntegral
  , count
  , derivative
  , iterFrom

  -- * Noise (random signal) sources and stochastic event sources
  , noise
  , noiseR
  , occasionally

  -- * Execution/simulation

  -- ** Reactimation
  , reactimate
  , ReactHandle
  , reactInit
  , react

  -- ** Embedding
  , embed
  , embedSynch
  , deltaEncode
  , deltaEncodeBy

  -- * AFRPV Wrapper Specific
  , renderNetwork
  , original

  -- * Auxiliary definitions
  , (#)
  , dup
  ) where

-----------------------------------------------------------------------------

import Control.Arrow
import FRP.Yampa.VectorSpace

import FRP.Yampa
  ( RandomGen(..)
  , Random(..)
  , Time
  , DTime
  , Event(..)
  , ReactHandle
  , noEvent
  , noEventFst
  , noEventSnd
  , event
  , fromEvent
  , maybeToEvent
  , isEvent
  , isNoEvent
  , tag
  , tagWith
  , attach
  , lMerge
  , rMerge
  , merge
  , mergeBy
  , mapMerge
  , mergeEvents
  , catEvents
  , joinE
  , splitE
  , filterE
  , mapFilterE
  , gate
  , react
  , deltaEncode
  , deltaEncodeBy
  , (#)
  , dup
  )

import qualified FRP.Yampa as Y
  ( SF
  , arrPrim
  , arrEPrim
  , identity
  , constant
  , localTime
  , time
  , (-->)
  , (-:>)
  , (>--)
  , (-=>)
  , (>=-)
  , initially
  , sscan
  , sscanPrim
  , never
  , now
  , after
  , repeatedly
  , afterEach
  , afterEachCat
  , delayEvent
  , delayEventCat
  , edge
  , iEdge
  , edgeTag
  , edgeJust
  , edgeBy
  , notYet
  , once
  , takeEvents
  , dropEvents
  , switch
  , dSwitch
  , rSwitch
  , drSwitch
  , kSwitch
  , dkSwitch
  , parB
  , pSwitchB
  , dpSwitchB
  , rpSwitchB
  , drpSwitchB
  , par
  , pSwitch
  , dpSwitch
  , rpSwitch
  , drpSwitch
  , hold
  , dHold
  , trackAndHold
  , accum
  , accumHold
  , dAccumHold
  , accumBy
  , accumHoldBy
  , dAccumHoldBy
  , accumFilter
  , pre
  , iPre
  , delay
  , pause
  , loopPre
  , loopIntegral
  , integral
  , imIntegral
  , impulseIntegral
  , count
  , derivative
  , iterFrom
  , noise
  , noiseR
  , occasionally
  , reactimate
  , reactInit
  , embed
  , embedSynch
  )

import AFRPV.SF.Observer
  ( ArrNet(..)
  , Observe(..)
  )

import AFRPV.SF.Yampa
  ( WSF
  , Delayed
  , Network
  , YampaPrimitive(..)
  , components
  )

import AFRPV.Render.Yampa
  ( renderNetwork
  )

-----------------------------------------------------------------------------

-- | Signal function that transforms a signal carrying values of some
-- type 'a' into a signal carrying values of some type 'b'. You can
-- think of it as (Signal a -> Signal b). A signal is, conceptually, a
-- function from 'Time' to value.

type SF = WSF

-----------------------------------------------------------------------------

infixr 0 -->, -:>, >--, -=>, >=-

-----------------------------------------------------------------------------

-- | Lifts a pure function into a signal function (applied pointwise).

arrPrim :: (a -> b) -> SF a b
arrPrim f = OA (Y.arrPrim f) Pure

-----------------------------------------------------------------------------

-- | Lifts a pure function into a signal function applied to events
-- (applied pointwise).

arrEPrim :: (Event a -> b) -> SF (Event a) b
arrEPrim f = OA (Y.arrEPrim f) Pure

-----------------------------------------------------------------------------

-- | Identity: identity = arr id
--
-- Using 'identity' is preferred over lifting id, since the arrow
-- combinators know how to optimise certain networks based on the
-- transformations being applied.

identity :: SF a a
identity = OA Y.identity Id

-----------------------------------------------------------------------------

-- | Identity: constant b = arr (const b)
--
-- Using 'constant' is preferred over lifting const, since the arrow
-- combinators know how to optimise certain networks based on the
-- transformations being applied.

constant :: b -> SF a b
constant x = OA (Y.constant x) $ Other $ Source "constant"

-----------------------------------------------------------------------------

-- | Outputs the time passed since the signal function instance was
-- started.

localTime :: SF a Time
localTime = OA Y.localTime $ Other $ Source "localTime"

-----------------------------------------------------------------------------

-- | Alternative name for localTime.

time :: SF a Time
time = OA Y.time $ Other $ Source "time"

-----------------------------------------------------------------------------

-- | Initialization operator (cf. Lustre/Lucid Synchrone).
--
-- The output at time zero is the first argument, and from
-- that point on it behaves like the signal function passed as
-- second argument.

(-->) :: b -> SF a b -> SF a b
(-->) x (OA a n) = OA ((Y.-->) x a) n

-----------------------------------------------------------------------------

-- | Output pre-insert operator.
--
-- Insert a sample in the output, and from that point on, behave
-- like the given sf.

(-:>) :: b -> SF a b -> SF a b
(-:>) x (OA a n) = OA ((Y.-:>) x a) n

-----------------------------------------------------------------------------

-- | Input initialization operator.
--
-- The input at time zero is the first argument, and from
-- that point on it behaves like the signal function passed as
-- second argument.

(>--) :: a -> SF a b -> SF a b
(>--) x (OA a n) = OA ((Y.>--) x a) n

-----------------------------------------------------------------------------

-- | Transform initial output value.
--
-- Applies a transformation 'f' only to the first output value at
-- time zero.

(-=>) :: (b -> b) -> SF a b -> SF a b
(-=>) f (OA a n) = OA ((Y.-=>) f a) n

-----------------------------------------------------------------------------

-- | Transform initial input value.
--
-- Applies a transformation 'f' only to the first input value at
-- time zero.

(>=-) :: (a -> a) -> SF a b -> SF a b
(>=-) f (OA a n) = OA ((Y.>=-) f a) n

-----------------------------------------------------------------------------

-- | Override initial value of input signal.

initially :: a -> SF a a
initially x = OA (Y.initially x) $ Other Initially

-----------------------------------------------------------------------------

-- | Applies a function point-wise, using the last output as next
-- input. This creates a well-formed loop based on a pure, auxiliary
-- function.

sscan ::  (b -> a -> b) -> b -> SF a b
sscan f x = OA (Y.sscan f x) $ Other $ Accum "sscan"

-----------------------------------------------------------------------------

-- | Generic version of 'sscan', in which the auxiliary function
-- produces an internal accumulator and an "held" output.
--
-- Applies a function point-wise, using the last known 'Just' output
-- to form the output, and next input accumulator. If the output is
-- 'Nothing', the last known accumulators are used. This creates a
-- well-formed loop based on a pure, auxiliary function.

sscanPrim :: (c -> a -> Maybe (c, b)) -> c -> b -> SF a b
sscanPrim f x y = OA (Y.sscanPrim f x y) $ Other $ Accum "sscanPrim"

-----------------------------------------------------------------------------

-- | Event source that never occurs.

never :: SF a (Event b)
never = OA Y.never $ Other $ Source "never"

-----------------------------------------------------------------------------

-- | Event source with a single occurrence at time 0. The value of the
-- event is given by the function argument.

now :: b -> SF a (Event b)
now x = OA (Y.now x) $ Other $ Source "now"

-----------------------------------------------------------------------------

-- | Event source with a single occurrence at or as soon after (local)
-- time /q/ as possible.

after :: Time -> b -> SF a (Event b)
after t x = OA (Y.after t x) $ Other $ Source "after"

-----------------------------------------------------------------------------

-- | Event source with repeated occurrences with interval q.  Note: If
-- the interval is too short w.r.t. the sampling intervals, the result
-- will be that events occur at every sample. However, no more than
-- one event results from any sampling interval, thus avoiding an
-- "event backlog" should sampling become more frequent at some later
-- point in time.

repeatedly :: Time -> b -> SF a (Event b)
repeatedly t x = OA (Y.repeatedly t x) $ Other $ Source "repeatedly"

-----------------------------------------------------------------------------

-- | Event source with consecutive occurrences at the given intervals.
-- Should more than one event be scheduled to occur in any sampling
-- interval, only the first will in fact occur to avoid an event
-- backlog.

afterEach :: [(Time, b)] -> SF a (Event b)
afterEach xs = OA (Y.afterEach xs) $ Other $ Source "afterEach"

-----------------------------------------------------------------------------

-- | Event source with consecutive occurrences at the given intervals.
-- Should more than one event be scheduled to occur in any sampling
-- interval, the output list will contain all events produced during
-- that interval.

afterEachCat :: [(Time, b)] -> SF a (Event [b])
afterEachCat xs = OA (Y.afterEachCat xs) $ Other $ Source "afterEachCat"

-----------------------------------------------------------------------------

-- | Delay for events. (Consider it a triggered after, hence /basic/.)

delayEvent :: Time -> SF (Event a) (Event a)
delayEvent t = OA (Y.delayEvent t) $ Other $ Delay "delayEvent"

-----------------------------------------------------------------------------

-- | Delay an event by a given delta and catenate events that occur so
-- closely so as to be /inseparable/.

delayEventCat :: Time -> SF (Event a) (Event [a])
delayEventCat t = OA (Y.delayEventCat t) $ Other $ Delay "delayEventCat"

-----------------------------------------------------------------------------

-- | A rising edge detector. Useful for things like detecting key
-- presses.  It is initialised as /up/, meaning that events occuring
-- at time 0 will not be detected.

edge :: SF Bool (Event ())
edge = OA Y.edge $ Other $ Trigger "edge"

-----------------------------------------------------------------------------

-- | A rising edge detector that can be initialized as up ('True',
--   meaning that events occurring at time 0 will not be detected) or
--   down ('False', meaning that events ocurring at time 0 will be
--   detected).

iEdge :: Bool -> SF Bool (Event ())
iEdge v = OA (Y.iEdge v) $ Other $ Trigger "iTrigger"

-----------------------------------------------------------------------------

-- | Like 'edge', but parameterized on the tag value.

edgeTag :: a -> SF Bool (Event a)
edgeTag v = OA (Y.edgeTag v) $ Other $ Trigger "edgeTag"

-----------------------------------------------------------------------------

-- | Edge detector particularized for detecting transtitions
--   on a 'Maybe' signal from 'Nothing' to 'Just'.

edgeJust :: SF (Maybe a) (Event a)
edgeJust = OA Y.edgeJust $ Other $ Trigger "edgeJust"

-----------------------------------------------------------------------------

-- | Edge detector parameterized on the edge detection function and
-- initial state, i.e., the previous input sample. The first argument
-- to the edge detection function is the previous sample, the second
-- the current one.

edgeBy :: (a -> a -> Maybe b) -> a -> SF a (Event b)
edgeBy f x = OA (Y.edgeBy f x) $ Other $ Trigger "edgeBy"

-----------------------------------------------------------------------------

-- | Suppression of initial (at local time 0) event.

notYet :: SF (Event a) (Event a)
notYet = OA Y.notYet $ Other $ Filter "notYet"

-----------------------------------------------------------------------------


-- | Suppress all but the first event.

once :: SF (Event a) (Event a)
once = OA Y.once $ Other $ Filter "once"

-----------------------------------------------------------------------------

-- | Suppress all but the first n events.

takeEvents :: Int -> SF (Event a) (Event a)
takeEvents x = OA (Y.takeEvents x) $ Other $ Filter "takeEvents"

-----------------------------------------------------------------------------

-- | Suppress first n events.

dropEvents :: Int -> SF (Event a) (Event a)
dropEvents x = OA (Y.dropEvents x) $ Other $ Filter "dropEvents"

-----------------------------------------------------------------------------

-- | Basic switch.
--
-- By default, the first signal function is applied. Whenever the
-- second value in the pair actually is an event, the value carried by
-- the event is used to obtain a new signal function to be applied *at
-- that time and at future times*. Until that happens, the first value
-- in the pair is produced in the output signal.
--
-- Important note: at the time of switching, the second signal
-- function is applied immediately. If that second SF can also switch
-- at time zero, then a double (nested) switch might take place. If
-- the second SF refers to the first one, the switch might take place
-- infinitely many times and never be resolved.
--
-- Remember: The continuation is evaluated strictly at the time of
-- switching!

switch
  :: SF a (b, Event c)
  -> (c -> SF a b)
  -> SF a b

switch (OA s x) f =
  OA (Y.switch s (arrow . f))
    $ Other $ Switch False x

-----------------------------------------------------------------------------

-- | Switch with delayed observation.
--
-- By default, the first signal function is applied.
--
-- Whenever the second value in the pair actually is an event, the
-- value carried by the event is used to obtain a new signal function
-- to be applied *at future times*.
--
-- Until that happens, the first value in the pair is produced in the
-- output signal.
--
-- Important note: at the time of switching, the second signal
-- function is used immediately, but the current input is fed by it
-- (even though the actual output signal value at time 0 is
-- discarded).
--
-- If that second SF can also switch at time zero, then a double
-- (nested) -- switch might take place. If the second SF refers to the
-- first one, the switch might take place infinitely many times and
-- never be resolved.
--
-- Remember: The continuation is evaluated strictly at the time of
-- switching!

dSwitch
  :: SF a (b, Event c)
  -> (c -> SF a b)
  -> SF a b

dSwitch (OA s x) f =
  OA (Y.dSwitch s (arrow . f))
    $ Other $ Switch True x

-----------------------------------------------------------------------------

-- | Recurring switch.
--
-- Uses the given SF until an event comes in the input, in which case
-- the SF in the event is turned on, until the next event comes in the
-- input, and so on.
--
-- See <https://wiki.haskell.org/Yampa#Switches> for more information
-- on how this switch works.

rSwitch
  :: SF a b
  -> SF (a, Event (SF a b)) b

rSwitch (OA s x) =
  OA (second (arr (fmap arrow)) >>> Y.rSwitch s)
    $ Other $ RSwitch False x

-----------------------------------------------------------------------------

-- | Recurring switch with delayed observation.
--
-- Uses the given SF until an event comes in the input, in which case
-- the SF in the event is turned on, until the next event comes in the
-- input, and so on.
--
-- Uses decoupled switch ('dSwitch').
--
-- See <https://wiki.haskell.org/Yampa#Switches> for more information
-- on how this switch works.

drSwitch
  :: SF a b
  -> SF (a, Event (SF a b)) b

drSwitch (OA s x) =
  OA (second (arr (fmap arrow)) >>> Y.drSwitch s)
    $ Other $ RSwitch True x

-----------------------------------------------------------------------------

-- | Call-with-current-continuation switch.
--
-- Applies the first SF until the input signal and the output signal,
-- when passed to the second SF, produce an event, in which case the
-- original SF and the event are used to build an new SF to switch
-- into.
--
-- See <https://wiki.haskell.org/Yampa#Switches> for more information
-- on how this switch works.

kSwitch
  :: SF a b
  -> SF (a, b) (Event c)
  -> (SF a b -> c -> SF a b)
  -> SF a b

kSwitch (OA a x) (OA t y) f =
  OA (Y.kSwitch a t g) $ Other $ KSwitch False x y

  where
    g b v = arrow $ f (OA b x) v

-----------------------------------------------------------------------------

-- | 'kSwitch' with delayed observation.
--
-- Applies the first SF until the input signal and the output signal,
-- when passed to the second SF, produce an event, in which case the
-- original SF and the event are used to build an new SF to switch
-- into.
--
-- The switch is decoupled ('dSwitch').
--
-- See <https://wiki.haskell.org/Yampa#Switches> for more
-- information on how this switch works.

dkSwitch
  :: SF a b
  -> SF (a, b) (Event c)
  -> (SF a b -> c -> SF a b)
  -> SF a b

dkSwitch (OA a x) (OA t y) f =
  OA (Y.dkSwitch a t g) $ Other $ KSwitch True x y

  where
    g b v = arrow $ f (OA b x) v

-----------------------------------------------------------------------------

-- | Spatial parallel composition of a signal function collection.
-- Given a collection of signal functions, it returns a signal
-- function that broadcasts its input signal to every element of the
-- collection, to return a signal carrying a collection of
-- outputs. See 'par'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>

parB
  :: Functor col
  => col (SF a b)
  -> SF a (col b)

parB c =
  OA (Y.parB $ fmap arrow c)
    $ Other $ Par $ fmap net c

-----------------------------------------------------------------------------

-- | Parallel switch (dynamic collection of signal functions spatially
-- composed in parallel) with broadcasting. See 'pSwitch'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>

pSwitchB
  :: Functor col
  => col (SF a b)
  -> SF (a, col b) (Event c)
  -> (col (SF a b) -> c -> SF a (col b))
  -> SF a (col b)

pSwitchB c (OA s x) f =
  OA (Y.pSwitchB (fmap arrow c) s g)
    $ Other $ PSwitch False (fmap net c) x

  where
    g y v = arrow $ f (fmap (`OA` Id) y) v

-----------------------------------------------------------------------------

-- | Decoupled parallel switch with broadcasting (dynamic collection
--   of signal functions spatially composed in parallel). See
--   'dpSwitch'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>

dpSwitchB
  :: Functor col
  => col (SF a b)
  -> SF (a, col b) (Event c)
  -> (col (SF a b) -> c -> SF a (col b))
  -> SF a (col b)

dpSwitchB c (OA s x) f =
  OA (Y.dpSwitchB (fmap arrow c) s g)
    $ Other $ PSwitch True (fmap net c) x

  where
    g y v = arrow $ f (fmap (`OA` Id) y) v

-----------------------------------------------------------------------------

-- | Recurring parallel switch with broadcasting.
--
-- Uses the given collection of SFs, until an event comes in the
-- input, in which case the function in the 'Event' is used to
-- transform the collections of SF to be used with 'rpSwitch' again,
-- until the next event comes in the input, and so on.
--
-- Broadcasting is used to decide which subpart of the input goes to
-- each SF in the collection.
--
-- See 'rpSwitch'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>

rpSwitchB
  :: Functor col
  => col (SF a b)
  -> SF (a, Event (col (SF a b) -> col (SF a b))) (col b)

rpSwitchB c =
  OA (second (arr (fmap g)) >>> Y.rpSwitchB (fmap arrow c))
    $ Other $ RPSwitch False $ fmap net c

  where
    g fi c = fmap arrow $ fi $ fmap (`OA` Id) c

-----------------------------------------------------------------------------

-- | Decoupled recurring parallel switch with broadcasting.
--
-- Uses the given collection of SFs, until an event comes in the
-- input, in which case the function in the 'Event' is used to
-- transform the collections of SF to be used with 'rpSwitch' again,
-- until the next event comes in the input, and so on.
--
-- Broadcasting is used to decide which subpart of the input goes to
-- each SF in the collection.
--
-- This is the decoupled version of 'rpSwitchB'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>

drpSwitchB
  :: Functor col
  => col (SF a b)
  -> SF (a, Event (col (SF a b) -> col (SF a b))) (col b)

drpSwitchB c =
  OA (second (arr (fmap g)) >>> Y.drpSwitchB (fmap arrow c))
    $ Other $ RPSwitch True $ fmap net c

  where
    g f c = fmap arrow $ f $ fmap (`OA` Id) c

-----------------------------------------------------------------------------

-- | Spatial parallel composition of a signal function collection parameterized
-- on the routing function.

par
  :: Functor col
  => (forall sf. a -> col sf -> col (b, sf))
  -> col (SF b c)
  -> SF a (col c)

par e c =
  OA (Y.par e $ fmap arrow c)
    $ Other $ Par $ fmap net c

-----------------------------------------------------------------------------

-- | Parallel switch parameterized on the routing function. This is
-- the most general switch from which all other (non-delayed) switches
-- in principle can be derived. The signal function collection is
-- spatially composed in parallel and run until the event signal
-- function has an occurrence. Once the switching event occurs, all
-- signal function are "frozen" and their continuations are passed to
-- the continuation function, along with the event value.

pSwitch
  :: Functor col
  => (forall sf. a -> col sf -> col (b, sf))
  -> col (SF b c)
  -> SF (a, col c) (Event d)
  -> (col (SF b c) -> d -> SF a (col c))
  -> SF a (col c)

pSwitch e c (OA s x) f =
  OA (Y.pSwitch e (fmap arrow c) s g)
    $ Other $ PSwitch False (fmap net c) x

  where
    g y v = arrow $ f (fmap (`OA` Id) y) v

-----------------------------------------------------------------------------


-- | Parallel switch with delayed observation parameterized on the
-- routing function.
--
-- The collection argument to the function invoked on the switching
-- event is of particular interest: it captures the continuations of
-- the signal functions running in the collection maintained by
-- 'dpSwitch' at the time of the switching event, thus making it
-- possible to preserve their state across a switch.  Since the
-- continuations are plain, ordinary signal functions, they can be
-- resumed, discarded, stored, or combined with other signal
-- functions.

dpSwitch
  :: Functor col
  => (forall sf. a -> col sf -> col (b, sf))
  -> col (SF b c)
  -> SF (a, col c) (Event d)
  -> (col (SF b c) -> d -> SF a (col c))
  -> SF a (col c)

dpSwitch e c (OA s x) f =
  OA (Y.dpSwitch e (fmap arrow c) s g)
    $ Other $ PSwitch True (fmap net c) x

  where
    g y v = arrow $ f (fmap (`OA` Id) y) v

-----------------------------------------------------------------------------

-- | Recurring parallel switch parameterized on the routing function.
--
-- Uses the given collection of SFs, until an event comes in the
-- input, in which case the function in the 'Event' is used to
-- transform the collections of SF to be used with 'rpSwitch' again,
-- until the next event comes in the input, and so on.
--
-- The routing function is used to decide which subpart of the input
-- goes to each SF in the collection.
--
-- This is the parallel version of 'rSwitch'.

rpSwitch
  :: Functor col
  => (forall sf. a -> col sf -> col (b, sf))
  -> col (SF b c)
  -> SF (a, Event (col (SF b c) -> col (SF b c))) (col c)

rpSwitch e c =
  OA (second (arr (fmap g)) >>> Y.rpSwitch e (fmap arrow c))
    $ Other $ RPSwitch False $ fmap net c

  where
    g f c = fmap arrow $ f $ fmap (`OA` Id) c

-----------------------------------------------------------------------------

-- | Recurring parallel switch with delayed observation parameterized
-- on the routing function.
--
-- Uses the given collection of SFs, until an event comes in the
-- input, in which case the function in the 'Event' is used to
-- transform the collections of SF to be used with 'rpSwitch' again,
-- until the next event comes in the input, and so on.
--
-- The routing function is used to decide which subpart of the input
-- goes to each SF in the collection.
--
-- This is the parallel version of 'drSwitch'.

drpSwitch
  :: Functor col
  => (forall sf. a -> col sf -> col (b, sf))
  -> col (SF b c)
  -> SF (a, Event (col (SF b c) -> col (SF b c))) (col c)

drpSwitch  e c =
  OA (second (arr (fmap g)) >>> Y.drpSwitch e (fmap arrow c))
    $ Other $ RPSwitch True $ fmap net c

  where
    g f c = fmap arrow $ f $ fmap (`OA` Id) c

-----------------------------------------------------------------------------

-- | Zero-order hold.
--
-- Converts a discrete-time signal into a continuous-time signal, by
-- holding the last value until it changes in the input signal. The
-- given parameter may be used for time zero, and until the first
-- event occurs in the input signal, so hold is always
-- well-initialized.
--
-- >>> embed (hold 1) (deltaEncode 0.1 [NoEvent, NoEvent, Event 2, NoEvent, Event 3, NoEvent])
-- [1,1,2,2,3,3]

hold :: a -> SF (Event a) a
hold x = OA (Y.hold x) $ Other $ Hold "hold"

-----------------------------------------------------------------------------

-- | Zero-order hold with a delay.
--
-- Converts a discrete-time signal into a continuous-time signal, by
-- holding the last value until it changes in the input signal. The
-- given parameter is used for time zero (until the first event occurs
-- in the input signal), so 'dHold' shifts the discrete input by an
-- infinitesimal delay.
--
-- >>> embed (dHold 1) (deltaEncode 0.1 [NoEvent, NoEvent, Event 2, NoEvent, Event 3, NoEvent])
-- [1,1,1,2,2,3]

dHold :: a -> SF (Event a) a
dHold x = OA (Y.dHold x) $ Other $ Hold "dHold"

-----------------------------------------------------------------------------

-- | Tracks input signal when available, holding the last value when
-- the input is 'Nothing'.
--
-- This behaves similarly to 'hold', but there is a conceptual
-- difference, as it takes a signal of input @Maybe a@ (for some @a@)
-- and not @Event@.
--
-- >>> embed (trackAndHold 1) (deltaEncode 0.1 [Nothing, Nothing, Just 2, Nothing, Just 3, Nothing])
-- [1,1,2,2,3,3]

trackAndHold :: a -> SF (Maybe a) a
trackAndHold x = OA (Y.trackAndHold x) $ Other $ Hold "trackAndHold"

-----------------------------------------------------------------------------

-- | Given an initial value in an accumulator, it returns a signal
-- function that processes an event carrying transformation functions.
-- Every time an 'Event' is received, the function inside it is
-- applied to the accumulator, whose new value is outputted in an
-- 'Event'.

accum :: a -> SF (Event (a -> a)) (Event a)
accum x = OA (Y.accum x) $ Other $ Accum "accum"

-----------------------------------------------------------------------------

-- | Zero-order hold accumulator (always produces the last outputted
-- value until an event arrives).

accumHold :: a -> SF (Event (a -> a)) a
accumHold x = OA (Y.accumHold x) $ Other $ Hold "accumHold"

-----------------------------------------------------------------------------

-- | Zero-order hold accumulator with delayed initialization (always
-- produces the last outputted value until an event arrives, but the
-- very initial output is always the given accumulator).

dAccumHold :: a -> SF (Event (a -> a)) a
dAccumHold x = OA (Y.dAccumHold x) $ Other $ Hold "dAccumHold"

-----------------------------------------------------------------------------

-- | Accumulator parameterized by the accumulation function.

accumBy :: (b -> a -> b) -> b -> SF (Event a) (Event b)
accumBy f x = OA (Y.accumBy f x) $ Other $ Accum "accumBy"

-----------------------------------------------------------------------------

-- | Zero-order hold accumulator parameterized by the accumulation
-- function.

accumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
accumHoldBy f x = OA (Y.accumHoldBy f x) $ Other $ Hold "accumHoldBy"

-----------------------------------------------------------------------------

-- | Zero-order hold accumulator parameterized by the accumulation
-- function with delayed initialization (initial output sample is
-- always the given accumulator).

dAccumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
dAccumHoldBy f x = OA (Y.dAccumHoldBy f x) $ Other $ Hold "dAccumHoldBy"

-----------------------------------------------------------------------------

-- | Accumulator parameterized by the accumulator function with
-- filtering, possibly discarding some of the input events based on
-- whether the second component of the result of applying the
-- accumulation function is 'Nothing' or 'Just' x for some x.

accumFilter :: (c -> a -> (c, Maybe b)) -> c -> SF (Event a) (Event b)
accumFilter f x = OA (Y.accumFilter f x) $ Other $ Accum "accumFilter"

-----------------------------------------------------------------------------

-- | Uninitialized delay operator.
--
-- The output has an infinitesimal delay (1 sample), and the value at
-- time zero is undefined.

pre :: SF a a
pre = OA Y.pre $ Other $ Delay "pre"

-----------------------------------------------------------------------------

-- | Initialized delay operator.
--
-- Creates an SF that delays the input signal, introducing an
-- infinitesimal delay (one sample), using the given argument to fill
-- in the initial output at time zero.

iPre :: a -> SF a a
iPre v = OA (Y.iPre v) $ Other $ Delay "iPre"

-----------------------------------------------------------------------------

-- | Delay a signal by a fixed time 't', using the second parameter to
-- fill in the initial 't' seconds.

delay :: Time -> a -> SF a a
delay t v =  OA (Y.delay t v) $ Other $ Delay "delay"

-----------------------------------------------------------------------------

-- | Given a value in an accumulator (b), a predicate signal function
-- (sfC), and a second signal function (sf), pause will produce the
-- accumulator b if sfC input is True, and will transform the signal
-- using sf otherwise.  It acts as a pause with an accumulator for the
-- moments when the transformation is paused.

pause :: b -> SF a Bool -> SF a b -> SF a b
pause v (OA a x) (OA b y) = OA (Y.pause v a b) $ Other $ Pause x y

-----------------------------------------------------------------------------

-- | Loop with an initial value for the signal being fed back.

loopPre :: c -> SF (a, c) (b, c) -> SF a b
loopPre v (OA a x) = OA (Y.loopPre v a) $ Loop x

-----------------------------------------------------------------------------

-- | Loop by integrating the second value in the pair and feeding the
-- result back. Because the integral at time 0 is zero, this is always
-- well defined.

loopIntegral :: VectorSpace c s => SF (a, c) (b, c) -> SF a b
loopIntegral (OA a x) = OA (Y.loopIntegral a) $ Loop x

-----------------------------------------------------------------------------

-- | Integration using the rectangle rule.

integral :: VectorSpace a s => SF a a
integral = OA Y.integral $ Other $ Accum "integral"

-----------------------------------------------------------------------------

-- | \"Immediate\" integration (using the function's value at the
-- current time)

imIntegral :: VectorSpace a s => a -> SF a a
imIntegral v = OA (Y.imIntegral v) $ Other $ Accum "imIntegral"

-----------------------------------------------------------------------------

-- | Integrate the first input signal and add the /discrete/
-- accumulation (sum) of the second, discrete, input signal.

impulseIntegral :: VectorSpace a k => SF (a, Event a) a
impulseIntegral = OA Y.impulseIntegral $ Other $ Accum "impulseIntegral"

-----------------------------------------------------------------------------

-- | Count the occurrences of input events.
--
-- >>> embed count (deltaEncode 1 [Event 'a', NoEvent, Event 'b'])
-- [Event 1,NoEvent,Event 2]

count :: Integral b => SF (Event a) (Event b)
count = OA Y.count $ Other $ Accum "count"

-----------------------------------------------------------------------------

-- | A very crude version of a derivative. It simply divides the value
-- difference by the time difference. Use at your own risk.

derivative :: VectorSpace a s => SF a a
derivative = OA Y.derivative $ Other $ Accum "derivative"

-----------------------------------------------------------------------------

-- | Integrate using an auxiliary function that takes the current and
-- the last input, the time between those samples, and the last
-- output, and returns a new output.

iterFrom :: (a -> a -> DTime -> b -> b) -> b -> SF a b
iterFrom f v = OA (Y.iterFrom f v) $ Other $ Accum "iterFrom"

-----------------------------------------------------------------------------

-- | Noise (random signal) with default range for type in question;
-- based on "randoms".

noise :: (RandomGen g, Random b) => g -> SF a b
noise x = OA (Y.noise x) $ Other $ Source "noise"

-----------------------------------------------------------------------------

-- | Noise (random signal) with specified range; based on "randomRs".

noiseR :: (RandomGen g, Random b) => (b, b) -> g -> SF a b
noiseR x y = OA (Y.noiseR x y) $ Other $ Source "noiseR"

-----------------------------------------------------------------------------

-- | Stochastic event source with events occurring on average once
-- every t_avg seconds. However, no more than one event results from
-- any one sampling interval in the case of relatively sparse
-- sampling, thus avoiding an "event backlog" should sampling become
-- more frequent at some later point in time.

occasionally :: RandomGen g => g -> Time -> b -> SF a (Event b)
occasionally x t y =
  OA (Y.occasionally x t y) $ Other $ Source "occasionally"

-----------------------------------------------------------------------------

-- | Convenience function to run a signal function indefinitely, using
-- a IO actions to obtain new input and process the output.
--
-- This function first runs the initialization action, which provides
-- the initial input for the signal transformer at time 0.
--
-- Afterwards, an input sensing action is used to obtain new input (if
-- any) and the time since the last iteration. The argument to the
-- input sensing function indicates if it can block. If no new input
-- is received, it is assumed to be the same as in the last iteration.
--
-- After applying the signal function to the input, the actuation IO
-- action is executed. The first argument indicates if the output has
-- changed, the second gives the actual output). Actuation functions
-- may choose to ignore the first argument altogether. This action
-- should return True if the reactimation must stop, and False if it
-- should continue.
--
-- Note that this becomes the program's /main loop/, which makes using
-- this function incompatible with GLUT, Gtk and other graphics
-- libraries. It may also impose a sizeable constraint in larger
-- projects in which different subparts run at different time
-- steps. If you need to control the main loop yourself for these or
-- other reasons, use 'reactInit' and 'react'.

reactimate
  :: Monad m
  => m a
  -> (Bool -> m (DTime, Maybe a))
  -> (Bool -> b -> m Bool)
  -> SF a b
  -> m ()

reactimate i s o (OA a _) = Y.reactimate i s o a

-----------------------------------------------------------------------------

-- | Initialize a top-level reaction handle.

reactInit
  :: IO a
  -> (ReactHandle a b -> Bool -> b -> IO Bool)
  -> SF a b
  -> IO (ReactHandle a b)

reactInit i u (OA a _) = Y.reactInit i u a

-----------------------------------------------------------------------------

-- | Given a signal function and a pair with an initial
-- input sample for the input signal, and a list of sampling
-- times, possibly with new input samples at those times,
-- it produces a list of output samples.
--
-- This is a simplified, purely-functional version of 'reactimate'.

embed :: SF a b -> (a, [(DTime, Maybe a)]) -> [b]
embed (OA a _) = Y.embed a

-----------------------------------------------------------------------------

-- | Synchronous embedding. The embedded signal function is run on the supplied
-- input and time stream at a given (but variable) ratio >= 0 to the outer
-- time flow. When the ratio is 0, the embedded signal function is paused.

embedSynch :: SF a b -> (a, [(DTime, Maybe a)]) -> SF Double b
embedSynch (OA a x) v = OA (Y.embedSynch a v) x

-----------------------------------------------------------------------------

-- | Extracts the original Yampa signal funcion from the extended
-- signal function wrapper.

original
  :: SF a b -> Y.SF a b

original = arrow

-----------------------------------------------------------------------------
