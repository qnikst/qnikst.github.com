----
title: Playing with types: type level numbers
author: Alexander Vershilov
tags: haskell, typefun, typenats
date: 2013-12-30
----


> As always I'm terribly sorry for my bad english, but writing
> the posts and having a feedback is the only reasonable way 
> to improve it.

This post describes an easy exercise that gives a feeling of 
use typelevel numbers. This post is a part of another work
that hopefully will be covered in following posts.

If you want to know more about typelevel literals and friends
you can take a look at the [following post](http://ponies.io/posts/2014-07-30-typelits.html)
it describes the theory much better than I can.

Intead of using type level Strings we will address numbers.
In this post I want to create a datatype that will contain a 
number that is known _statically_ (at compilation time) and which
can be extracted from type at _runtime_.

The first part of the problem (having a number) in a type may
be interesting if you want to distiguish types assosiated with
different numbers.

For simplicity we will take [Data.Tagged.Tagged](https://hackage.haskell.org/package/tagged-0.7.2/docs/Data-Tagged.html)
type from `tagged` package. However it's possible to create our
own type as I did at the beggining (just I did a first time).

We add helper methods that allow us to extract infomation
about the type-parameter from the value:

```{.haskell}
toProxy :: Tagged n a  -> Proxy n
toProxy _ = Proxy
``````````````````````````````````

And add a constrictor helper that allow to create a type with 
information about number that is provided by us.

```{.haskell}
mkT :: proxy n -> a -> Tagged n a
mkT _ a = Tagged a
``````````````````````````````````

Just check that we can create a fancy values:

```{.haskell}
*Main> :t mkT (Proxy :: Proxy Nothing) 8
mkT (Proxy :: Proxy Nothing) 8 :: Num a => Tagged 'Nothing a
`````````````````````````````````````````````````````````````

# Naturals

From this point we can start real fun.
At first lest create a value with natural type parameter:

```{.haskell}
*Main> :t mkT (Proxy :: Proxy 3) 8
mkT (Proxy :: Proxy 3) 8 :: Num a => Tagged 3 a
````````````````````````````````````````````````

Now we want to be able to use this information at runtime.
Let's introduce a new method that will use `natVal` function:

````{.haskel}
natVal :: forall n proxy. KnownNat n => proxy n -> Integer` 
```````````````````````````````````````````````````````````

take a type that acts as a type proxy and returns the integer
assosiated with a type.

```{.haskell}
useT :: (KnownNat n, Num a) => Tagged n a -> a
useT t@(Tagged a) = fromIntegral (natVal (toProxy t)) + a
``````````````````````````````````````````````````````````

Here we are:

  1. extracting type information by calling `toProxy`

  2. extracting natural number from a proxy by calling `natVal`

  3. returning a calculated results

```{.haskell}
*Main> useT $ mkT (Proxy :: Proxy 3) 8
11
```````````````````````````````````````

# Positive rational

On the next step we will introduce a code that works for natural numbers,
by definition rational number is `a % b`, where `a \in Z` and `b \in N`.
Lets lift this information to a type level by introducing a typelevel
rational:

```{.haskell}
data (:%%) a b
````````````````

And introduce type extraction mechanisms

````` haskell
-- | extract numerator type
numerator :: proxy (n :%% m) -> Proxy n
numerator _ = Proxy

-- | Extract denomenator type 
denomenator :: proxy (n :%% m)  -> Proxy m
denomenator _ = Proxy
```````````````````````````````````````````````

Today we don't conver math operations over the type-parameters, this may
be a topic for another post. So now we may be quite happy as we can 
create values tagged by something that looks like a rational number.

``````````{.haskell}
*Main> :t mkT (Proxy :: Proxy (3 :%% 8)) 8
mkT (Proxy :: Proxy (3 :%% 8)) 8 :: Num a => Tagged (3 :%% 8) a
`````````````````````````````````````````````````````````````````

Now we can write a function that will use information about the type at runtime:

````````````{.haskell}
useTF :: (KnownNat n, KnownNat m, Fractional a) => Tagged (n :%% m) a -> a
useTF t@(Tagged a) = v + a
  where v = fromRational $
               natVal (numerator $ toProxy t) :% natVal (denomenator $ toProxy t)
```````````````````````````````````````````````````````````````````````````````

Lets check:

```````````{.haskell}
*Main> useTF $ mkT (Proxy :: Proxy (3 :%% 8)) 8
8.375
````````````````````````````````````````````````


# Positive real.

Now we want to solve the following problem: it's not easy to write types for 
ratio, the reason that for a complex cases we need somehow to find the ratio that
fits. Also we want to support real numbers. Having in mind the fact that real values
are not supported on the typelevel,  we may be quite happy with Rational
approximation.

In order to solve those 2 problems we need:

  1. real values support

  2. ability to write code easily

We will introduce a TH function that will find a good approximation and create
required type:


````````````{.haskell}
mkFloatProxy :: RealFrac a => a -> Q Exp
mkFloatProxy x = [| Proxy :: Proxy ($(nk a) :%% $(nk b)) |]
  where (a :% b) = toRational x
          nk x = sigT (litT (numTyLit x)) (ConT $ mkName "Nat")
`````````````````````````````````````````````````````````````````

the magic that we have in `nk x` we need to create a type parameter of kind
`Nat`. Now we can write  a code like:


> test = mkT $(mkFloatProxy pi) 7.0


This is quite nice, next steps may be support of negative values, using SomeNat
and operations on values.
