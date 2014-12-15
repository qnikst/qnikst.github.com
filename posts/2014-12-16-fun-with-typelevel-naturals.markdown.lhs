For time ago Bas van Dijk asked a question on glasgow-haskell-users mailing
list about [proving the properties of type-level natural numbers obtained by user](https://www.haskell.org/pipermail/glasgow-haskell-users/2014-November/025451.html). That lead to an interesting discussion, that you can
read in the list archives.

The problem was that we want to impose a constraint on some typelevel natural
and then read value from user and guarantee that we have that constraint.
For such constraint we took (<=255).

Here is a file that describe few approaches to solve this problem.

At first we need to use a HUGE list of pragmas.

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE ViewPatterns #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE ConstraintKinds #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

And relax compiler options a bit.

> {-# OPTIONS_GHC -ftype-function-depth=1024 #-}
> {-# OPTIONS_GHC -fcontext-stack=1024 #-}
>
> import GHC.TypeLits
> import GHC.Exts 
> import Data.Proxy
> import Data.Constraint
> import Data.Type.Bool
> import Data.Type.Equality
> import System.Environment
> import Unsafe.Coerce
> import Data.Singletons
> import Control.Monad


Attempt-1: Tagged value
------------------------------------------------------------------------

We can keep a proof together with value, so we can introduce a new type:

> data Proof n  (c :: Constraint) where Proof :: Proxy n -> Proof n c


In mailing list I provide following code:

> fromSomeError :: SomeNat -> Maybe (Proof n (n <= 255))
> fromSomeError (SomeNat p)
>    | natVal p <= 255 = Just (Proof (Proxy :: Proxy n))
>    | otherwise = Nothing



This is obviously a bug as we check natVal p, but return Proxy n.
And correct version fails, as there is no way to simply inject a constraint.

> {-
> fromSome :: SomeNat -> Maybe (Proof n (n <= 255))
> fromSome (SomeNat p)
>   | natVal p <= 255 = Just (Proof p)
>   | otherwise = Nothing
> -}

~~~~~~~~~~~~~~
 Proof.lhs:53:37:
    Could not deduce (n1 ~ n)
    from the context (KnownNat n1)
    bound by a pattern with constructor
        SomeNat :: forall (n :: Nat). KnownNat n => Proxy n -> SomeNat,
	in an equation for ‘fromSome’
	at Proof.lhs:52:13-21
~~~~~~~~~~~~~~~


Attempt-2: Type carrier
----------------------------------------------------------------------

Now we want to have the following constraint on out Nat

> data Proof2 :: (Nat -> Constraint) -> * where
>   Proof2 :: c n => Proxy n -> Proof2 c

In order to have a 'curried' version of our constaint we can introduce
a type family

> type family LessThan255 n :: Constraint where
>     LessThan255 f = (f <= 255)

Now lets try to create a proof from known natural:


> {-
> strange :: (LessThan255 n) => Proxy n -> Proof2 LessThan255
> strange = Proof2
> -}

~~~~~~~~~~~~~~~
Proof.lhs:89:13:
    Could not deduce (LessThan255 n) arising from a use of ‘Proof2’
    from the context (LessThan255 n)
    bound by the type signature for
	strange :: (LessThan255 n) => Proxy n -> Proof2 LessThan255
~~~~~~~~~~~~~~


> strange :: (LessThan255 ~ c, c n) => Proxy n -> Proof2 c
> strange = Proof2

Previous attempt to build function failed, but this one works. I don't
know if it's related to non-injectivity or a bug.. I can report it :)

When I realized this problem, I have stopped.

Update. As Richard Eisenberg says:

    By the way, the bug in the Proof2 version is a bug in GHC 7.8.3
    (only in .3 -- not in .2 or in the soon-to-be .4) that allows you
    to write unsaturated type families that don't work.
    Saying `LessThan255` without a parameter should be a syntax error,
    but that check was accidentally turned off for 7.8.3, leading to a bogus type error.

Attempt-3: carry a contraint in a datatype
----------------------------------------------------------------------

Now let's keep our 'constraint' in a datatype, here we have 2 proxy,
one for datatype, and one for value, also a KnownNat constraint that
we want to use later:

> data Proof3 :: (Nat -> *) -> * where
>   Proof3 :: KnownNat n => c n -> Proxy n -> Proof3 c

We can introduce a Show instance

> instance Show (Proof3 c) where
>   show (Proof3 _ k) = show $ natVal k

And now we can introduce a LessThen constraint as a datatype:

> data LessThan255D (n::Nat) where LessThan255D :: (n <= 255) => LessThan255D n

We can convert  type level constrant to data easily:

> c2d :: LessThan255 n => Proxy n -> LessThan255D n
> c2d _ = LessThan255D

But what about a proof, ideally we want to have following code:

> {-
> fromSome3 :: SomeNat -> Maybe (Proof3 LessThan255D)
> fromSome3 (SomeNat p)
>    | natVal p < natVal t255 = Just (Proof3 LessThan255D p)
>    | otherwise = Nothing
>    where t255 = Proxy :: Proxy 255
> -}

    Proof.lhs:138:46:
      Could not deduce ((n <=? 255) ~ 'True)
      from the context (KnownNat n)

It doesn't work simply because value level check doesn't guarantee
typelevel properties.

One way to solve it is to use unsafeCoerce: the idea is to use
a proof for the value we know, and then coerce a type of a proof
to the type of the proof related to the users value:

> fromSome3 :: SomeNat -> Maybe (Proof3 LessThan255D)
> fromSome3 (SomeNat p)
>    | natVal p < natVal t255 = Just (fake (c2d t255) p)
>    | otherwise = Nothing
>    where t255 = Proxy :: Proxy 255
>          fake :: KnownNat n => c p -> Proxy n -> Proof3 c
>          fake k p = Proof3 (unsafeCoerce k :: c n) p

It's a nice solution, it's unsafe as typechecker does not
check that our predicate `(natVal (p::Proxy n) < natVal (t255::Proxy 255))`
implies the safety of a coerce from `LessThan255 ~ 255 -> LessThan255 ~ p`.


There is one more solution that is really typesafe, but has a big
complexity, and require a bounded set of values:

We can try to check is equal to one specific value from the set.

> guessProof :: (KnownNat n, n <= 255) => SomeNat -> Proxy n -> Maybe (Proof3 LessThan255D)
> guessProof (SomeNat p) n = case sameNat p n of
>     Just _  -> Just $ Proof3 LessThan255D n
>     Nothing -> Nothing

Now we can build all set of values that are good:

> type family Guesses (n::Nat) :: [Nat] where
>    Guesses 0 = '[0]
>    Guesses n = n ': Guesses (n-1)

And write a code that will check all possible values:

> class GuessProof (n :: [Nat]) where
>   proof :: SomeNat -> Proxy n -> Maybe (Proof3 LessThan255D)
>
> instance GuessProof '[] where
>   proof _ _ = Nothing
>
> instance (KnownNat n, n <= 255, GuessProof ns) => GuessProof (n ': ns) where
>   proof s p = guessProof s (inner p) `mplus` proof s (next p)
>    where inner :: Proxy (n ': ns) -> Proxy (n::Nat)
>          inner _ = Proxy
>          next :: Proxy (n ': ns) -> Proxy (ns::[Nat])
>          next _ = Proxy

It's not very usable, have a bad complexity and require to change ghc options
but it works and it's safe.

Now let's test our code:

> f2 :: (c ~ (n <= 255)) => Proof3 LessThan255D -> IO ()
> f2 (Proof3 n p) = print $ natVal p

> test1 :: IO ()
> test1 = do
>     n <- readLn :: IO Integer
>
>     case someNatVal n of
>       Nothing -> error "Input is not a natural number!"
>       Just sn -> case fromSome3 sn of
>                    Just p -> f2 p
>                    _ -> error "Input if larger than 255"
> 

> test2 :: IO ()
> test2 = do
>   n <- readLn :: IO Integer
>   case someNatVal n of
>       Nothing -> error "Input is not a natural number!"
>       Just sn -> case proof sn (g (Proxy :: Proxy 255)) of
>                    Just p -> f2 p
>                    _ -> error "Input if larger than 255"
>   where
>     g :: Proxy n -> Proxy (Guesses n)
>     g _ = Proxy


Attempt 5: Singletons
---------------------

This is not the only solution, one more solution was provided by Richard Eisenberg
I have not found it in mailing list archives so including it here.

The idea is that we may use less effective representation for typelevel naturals, 
i.e. unary naturals we can build a proof using singletons:

> {-# LANGUAGE TemplateHaskell, DataKinds, PolyKinds, TypeFamilies,
>              ScopedTypeVariables, TypeOperators, UndecidableInstances,
>              GADTs, RankNTypes #-}
> {-# OPTIONS_GHC -ftype-function-depth=300 -fcontext-stack=300 #-}
>
> import Data.Singletons.TH
> import GHC.TypeLits hiding ( Nat )
>
> $(singletons [d|
>   data Nat = Zero | Succ Nat
>
>   leNat :: Nat -> Nat -> Bool
>   leNat Zero     _        = True
>   leNat (Succ _) Zero     = False
>   leNat (Succ a) (Succ b) = a `leNat` b
>   |])
>
> -- | Singletons's 'withSomeSing' is what we want, but a bug in 7.8.3 doesn't
> -- let it work without a specialized type for 'Nat's
> withSomeNat :: Nat -> (forall (n :: Nat). Sing n -> r) -> r
> withSomeNat = withSomeSing
>
> -- | Conveniently generate unary naturals
> type family U n where
>   U 0 = Zero
>   U n = Succ (U (n-1))
>
> toNat :: Integer -> Maybe Nat
> toNat n | n < 0      = Nothing
>         | otherwise  = Just $ go n
>   where
>     go 0 = Zero
>     go n = Succ (go (n-1))
>
> type Bound = U 255
>
> -- easier to test in GHCi than a proper 'main'
> go :: Integer -> IO ()
> go n =
>    case toNat n of
>      Nothing  -> putStrLn "Input is not a natural number!"
>      Just nat -> putStrLn $ withSomeNat nat $ \ snat ->
>        case snat `sLeNat` (sing :: Sing Bound) of
>          STrue  -> f snat
>          SFalse -> "Didn't work"
>
> f :: forall proxy (n :: Nat). (n `LeNat` Bound) ~ True => proxy n -> String
> f _ = "It worked!"


Attempt 6: Using Luquid haskell
-------------------------------

Ranjit Jhala, provided a great solution for liquid haskell, that could
be found either by [url](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1418064183.hs)
or here:

-----
> 
> {-@ LIQUID "--no-termination" @-}
> 
> module Nat255 where
> 
> -- | Define a predicate for valid integers
> 
> {-@ predicate IsValid X = 0 <= X && X < 255 @-}
> 
> -- | Use the predicate to define a refinement type (subset) of valid integers
> 
> {-@ type Valid = {v:Int | IsValid v}        @-}
> 
> -- | A function that checks whether a given Int is indeed valid
> 
> {-@ isValid   :: n:Int -> {v:Bool | Prop v <=> IsValid n} @-}
> isValid n     = 0 <= n && n < (255 :: Int)
> 
> -- | A function that can only be called with Valid Ints.
> 
> {-@ workWithValidNumber :: Valid -> IO () @-} 
> workWithValidNumber n = putStrLn $ "This is a valid number" ++ show (n :: Int)
> 
> -- | This is fine...
> ok    = workWithValidNumber 12
> 
> -- | ... But this is not.        
> notOk = workWithValidNumber 257
> 
> -- | Finally the top level loop that inputs a number, tests it 
> --   and calls `workWithValidNumber` if the number is valid.
> 
> loop = do putStrLn "Enter Number between 0 and 255"
>           n <- readLn :: IO Int
>           if isValid n
>              then workWithValidNumber n
>              else putStrLn "Humph, bad input, try again!" >> loop


I hope you have enjoyed reading this. 
