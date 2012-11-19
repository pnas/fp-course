module L03.StateT where

import L01.Id
import L01.Optional
import L02.List
import L03.Fluffy
import L03.Misty
import L03.State
import qualified Data.Set as S
import qualified Data.Foldable as F

-- A `StateT` is a function from a state value `s` to a functor of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- Exercise 1
-- Relative Difficulty: 2
-- Implement the `Fluffy` instance for `StateT s f` given a Fluffy f.
instance Fluffy f => Fluffy (StateT s f) where
  furry f (StateT k) =
    StateT (furry (\(a, t) -> (f a, t)) . k)

-- Exercise 2
-- Relative Difficulty: 5
-- Implement the `Misty` instance for `StateT s g` given a Misty f.
-- Make sure the state value is passed through in `banana`.
instance Misty f => Misty (StateT s f) where
  banana f (StateT k) =
    StateT (banana (\(a, t) -> runStateT (f a) t) . k)
  unicorn a =
    StateT (\s -> unicorn (a, s))

-- A `State'` is `StateT` specialised to the `Id` functor.
type State' s a =
  StateT s Id a

-- Exercise 3
-- Relative Difficulty: 1
-- Provide a constructor for `State'` values.
state' ::
  (s -> (a, s))
  -> State' s a
state' k =
  StateT (Id . k)

-- Exercise 4
-- Relative Difficulty: 1
-- Provide an unwrapper for `State'` values.
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' (StateT k) =
  runId . k

-- Exercise 5
-- Relative Difficulty: 2
-- Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Fluffy f =>
  StateT s f a
  -> s
  -> f s
execT (StateT k) =
  furry snd . k

-- Exercise 6
-- Relative Difficulty: 1
-- Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' t =
  runId . execT t

-- Exercise 7
-- Relative Difficulty: 2
-- Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Fluffy f =>
  StateT s f a
  -> s
  -> f a
evalT (StateT k) =
  furry fst . k

-- Exercise 8
-- Relative Difficulty: 1
-- Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' t =
  runId . evalT t

-- Exercise 9
-- Relative Difficulty: 2
-- A `StateT` where the state also distributes into the produced value.
getT ::
  Misty f =>
  StateT s f s
getT =
  StateT (\s -> unicorn (s, s))

-- Exercise 11
-- Relative Difficulty: 2
-- A `StateT` where the resulting state is seeded with the given value.
putT ::
  Misty f =>
  s
  -> StateT s f ()
putT =
  StateT . const . unicorn . (,) ()

-- Exercise 12
-- Relative Difficulty: 4
-- Remove all duplicate elements in a `List`.
-- ~~~ Use filterM and State' with a Data.Set#Set. ~~~
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' x =
  eval' (filterM (\a -> state' (\s -> (a `S.notMember` s, a `S.insert` s))) x) S.empty

-- Exercise 13
-- Relative Difficulty: 5
-- Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
-- ~~~ Use filterM and StateT over Optional with a Data.Set#Set. ~~~
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF x =
  evalT (filterM (\a -> StateT (\s ->
    if a > 100 then Empty else Full (a `S.notMember` s, a `S.insert` s))) x) S.empty

-- An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- Exercise 14
-- Relative Difficulty: 3
-- Implement the `Fluffy` instance for `OptionalT f` given a Fluffy f.
instance Fluffy f => Fluffy (OptionalT f) where
  furry f (OptionalT x) =
    OptionalT (furry (furry f) x)

-- Exercise 15
-- Relative Difficulty: 5
-- Implement the `Misty` instance for `OptionalT f` given a Misty f.
instance Misty f => Misty (OptionalT f) where
  unicorn =
    OptionalT . unicorn . unicorn
  banana f (OptionalT x) =
    OptionalT (banana (\o -> case o of
                               Empty -> unicorn Empty
                               Full a -> runOptionalT (f a)) x)

-- Exercise 16
-- Relative Difficulty: 7
-- Remove all duplicate elements in a `List`.
-- However, if you see a value greater than the given parameter in the list,
-- abort the computation by producing `Empty`.
-- ~~~ Use filterM and StateT over (OptionalT over ((->) a) with a Data.Set#Set. ~~~
distinctG ::
  (Ord a, Num a) =>
  List a
  -> a
  -> Optional (List a)
distinctG x =
  runOptionalT (evalT (filterM (\a -> StateT (\s ->
    OptionalT (\q -> if a > q then Empty else Full (a `S.notMember` s, a `S.insert` s)))) x) S.empty)

-- A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger [l] a
  deriving (Eq, Show)

-- Exercise 17
-- Relative Difficulty: 4
-- Implement the `Fluffy` instance for `Logger`.
instance Fluffy (Logger l) where
  furry f (Logger l a) =
    Logger l (f a)

-- Exercise 18
-- Relative Difficulty: 5
-- Implement the `Misty` instance for `Logger`.
-- The `banana` implementation must append log values to maintain associativity.
instance Misty (Logger l) where
  unicorn =
    Logger []
  banana f (Logger l a) =
    let Logger l' b = f a
    in Logger (l ++ l') b

-- Exercise 19
-- Relative Difficulty: 1
-- A utility function for producing a `Logger` with one log value.
log1 ::
  l
  -> a
  -> Logger l a
log1 l =
  Logger [l]

-- Exercise 20
-- Relative Difficulty: 10
-- Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
-- ~~~ Use filterM and StateT over (OptionalT over Logger with a Data.Set#Set. ~~~
distinctH ::
  (Integral a) =>
  List a
  -> Logger String (Optional (List a))
distinctH x =
  runOptionalT (evalT (filterM (\a -> StateT (\s ->
    OptionalT (if a > 100
                 then
                   log1 ("aborting > 100: " ++ show a) Empty
                 else (if even a
                   then log1 ("even number: " ++ show a)
                   else unicorn) (Full (a `S.notMember` s, a `S.insert` s))))) x) S.empty)