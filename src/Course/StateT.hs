{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}


module Course.StateT where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) ::
    (a -> b)
    -> StateT s f a
    -> StateT s f b
  (<$>) t (StateT r) = StateT $ \ s -> 
                                let fstSt = r s
                                    sndSt = (<$>) ( \ ( x , d )  -> ( t x , d)) fstSt
                                    in sndSt

-- error "todo: Course.StateT (<$>)#instance (StateT s f)"

-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> runStateT (StateT (\s -> Full ((+2), s ++ (1:.Nil))) <*> (StateT (\s -> Full (2, s ++ (2:.Nil))))) (0:.Nil)
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s ++ (1:.Nil)) :. ((+3), s ++ (1:.Nil)) :. Nil) <*> (StateT (\s -> (2, s ++ (2:.Nil)) :. Nil))) (0:.Nil)
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where
  pure ::
    a
    -> StateT s f a
  pure w = StateT $ \x -> lift0 (w,x)
    -- error "todo: Course.StateT pure#instance (StateT s f)"
    
  (<*>) ::
    StateT s f (a -> b)
    -> StateT s f a
    -> StateT s f b
  (<*>) (StateT a1) ( StateT s1 ) = StateT $ \x -> do 
    (c1 , x') <- a1 x
    (f1 , y') <- s1 x'
    pure ((c1 f1) , y' )

    -- error "todo: Course.StateT (<*>)#instance (StateT s f)"

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  (=<<) ::
    (a -> StateT s f b)
    -> StateT s f a
    -> StateT s f b
  (=<<) f (StateT x) =  StateT $ \s -> do
    (v,s') <- x s          -- get new value and state
    runStateT (f v) s'     -- pass them to f

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a =
  StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' x = StateT $ \s -> let (v , s' ) = x s
  in 
    ExactlyOne ( v , s')
   
-- error "todo: Course.StateT#state'"
-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' r u  = runExactlyOne $ runStateT r u

  -- error "todo: Course.StateT#runState'"
  

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
--
-- >>> execT (StateT $ \s -> Full ((), s + 1)) 2
-- Full 3
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT r u = ( \ (_ , v) -> v ) <$> runStateT r u
  -- error "todo: Course.StateT#execT"

-- | Run the `State'` seeded with `s` and retrieve the resulting state.
--
-- >>> exec' (state' $ \s -> ((), s + 1)) 2
-- 3
exec' ::
  State' s a
  -> s
  -> s
exec' r u = snd $ runState' r u
  -- error "todo: Course.StateT#exec'"

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
--
-- >>> evalT (StateT $ \s -> Full (even s, s + 1)) 2
-- Full True
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT r u = ( \ (w , _) -> w ) <$> runStateT r u
  -- error "todo: Course.StateT#evalT"

-- | Run the `State'` seeded with `s` and retrieve the resulting value.
--
-- >>> eval' (state' $ \s -> (even s, s + 1)) 5
-- False
eval' ::
  State' s a
  -> s
  -> a
eval' r u = fst $ runState' r u
  -- error "todo: Course.StateT#eval'"

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Applicative f =>
  StateT s f s
getT = StateT $ \s -> pure (s, s)
  -- error "todo: Course.StateT#getT"

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Applicative f =>
  s
  -> StateT s f ()
putT s = StateT $ \_ -> pure ((), s)

  -- error "todo: Course.StateT#putT"

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  Ord a =>
  List a
  -> List a
distinct' z = eval (filtering (\x -> State $ \s -> (S.notMember x s, S.insert x s)) z) S.empty

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF z = evalT (filtering (\x  -> StateT $ \s -> 
  if (x > 100)
    then Empty 
    else Full (S.notMember x s, S.insert x s)) z) S.empty

  -- error "todo: Course.StateT#distinctF"  
  -- 

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  (<$>) ::
    (a -> b)
    -> OptionalT f a
    -> OptionalT f b
  (<$>) t (OptionalT oft) = OptionalT $ lift1 (\x -> t x) <$> oft

--  error "todo: Course.StateT (<$>)#instance (OptionalT f)"

-- | Implement the `Applicative` instance for `OptionalT f` given a Monad f.
--
-- /Tip:/ Use `onFull` to help implement (<*>).
--
-- >>> runOptionalT $ OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- []
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
-- []
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty,Empty]
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Full 2,Full 3,Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Monad f => Applicative (OptionalT f) where
  pure ::
    a
    -> OptionalT f a
  pure z = OptionalT $ lift0 (pure z)  -- f (Optional a)
    -- error "todo: Course.StateT pure#instance (OptionalT f)"

  (<*>) ::
    OptionalT f (a -> b)
    -> OptionalT f a
    -> OptionalT f b
  (<*>) (OptionalT e) (OptionalT c) = OptionalT $  do
      r <- e
      s <- c
      onFull (\z -> pure (r <*> (lift0 z) )) s
      -- pure (r <*> s)
      --- alt1 solution --- correct
      -- OptionalT f <*> OptionalT a =
      --   OptionalT (lift2 (<*>) f a)

  -- alt2 solution wrong
  -- (<*>) e c = do 
  --   r <- e
  --   w <- c
  --   let zz = r w
  --   OptionalT $ lift0 (pure zz)

-- error "todo: Course.StateT (<*>)#instance (OptionalT f)"

-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  (=<<) ::
    (a -> OptionalT f b)
    -> OptionalT f a
    -> OptionalT f b
  (=<<) z (OptionalT y) = OptionalT $ do 
    r <- y
    case r of
      Empty -> pure Empty
      Full e -> runOptionalT (z e)

    -- -- other solution -- --
    -- OptionalT ((\o -> case o of
    -- Empty -> pure Empty
    -- Full a -> runOptionalT (z a)) =<< y)

-- error "todo: Course.StateT (=<<)#instance (OptionalT f)"

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) ::
    (a -> b)
    -> Logger l a
    -> Logger l b
  (<$>) f (Logger lst ab) = Logger lst (f ab)

    -- error "todo: Course.StateT (<$>)#instance (Logger l)"

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure ::
    a
    -> Logger l a
  pure z = Logger Nil z
    -- error "todo: Course.StateT pure#instance (Logger l)"

  (<*>) ::
    Logger l (a -> b)
    -> Logger l a
    -> Logger l b
  (<*>) (Logger l1 f) (Logger l2 u) = Logger (l1 ++ l2) (f u)
    -- error "todo: Course.StateT (<*>)#instance (Logger l)"

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  (=<<) ::
    (a -> Logger l b)
    -> Logger l a
    -> Logger l b
  (=<<) f (Logger l1 it ) = let (Logger l2 rr) = f it
    in 
      Logger (l1 ++ l2) rr 

    -- error "todo: Course.StateT (=<<)#instance (Logger l)"

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 v it = Logger (v :. Nil) it 
  -- error "todo: Course.StateT#log1"

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG z = runOptionalT (evalT (filtering (\a -> StateT (\s ->
  OptionalT (if a > 100
               then
                 log1 (fromString ("aborting > 100: " P.++ show a)) Empty
               else (if even a
                 then log1 (fromString ("even number: " P.++ show a))
                 else pure) (Full (a `S.notMember` s, a `S.insert` s))))) z) S.empty)



-- plain wrong , edit to fix
-- runOptionalT $ evalT (filtering (\x -> StateT $ \ (s, ll :: Chars) -> 
--   if (x > 100)
--     then 
--       OptionalT $ Logger ( "aborting > 100" :. ll) Empty
--   else if (even x)
--     then
--       OptionalT $ Logger ll (Full (S.notMember x s, ( S.insert x s ,"even" :. ll) ))
--   else 
--     OptionalT $ Logger ll (Full (S.notMember x s, ( S.insert x s ,ll) ) ) )  z  ) (S.empty , Nil :: Chars )

  -- error "todo: Course.StateT#distinctG"

onFull ::
  Applicative f =>
  (t -> f (Optional a))
  -> Optional t
  -> f (Optional a)
onFull g o =
  case o of
    Empty ->
      pure Empty
    Full a ->
      g a
