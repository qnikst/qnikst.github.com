Code and math in this post is done with the great help of Korolyov V.
This post is written as a literate haskell file so it's possible 
to copy it in a text file and run it in ghci. All sources can
be found in [haskell-fun](https://github.com/qnikst/haskell-fun/tree/master/series)
repository.

> {-# LANGUAGE BangPatterns #-}
> import Control.Monad.Fix

We want to implement a with formal series expansion using haskell.
Formal series can be expressed as follows:

$$ f(x) = \sum\limits_{i=0}^\infty a_ix^i $$

And also address a Taylor series as an example. For a function $f$ we
can expand it an any given point $x_0$, then:

$$ f(x) = f(x_0) + \sum_{k=1}\frac{f^{(k)}}{k!}(x-x_0)^k $$

Here we wanto to introduce a data structure that is capable for
representation of such series. For such datastructure we have
2 main candidates:

  1. List - a datastructure with two constructors that represent
       a possibly infinite single-linked list.

  2. Stream - a datastructure with one constructor that represent
       an infinite stream of values.

We may want to select a list because this way we may represent
a finite series (as some functions have all coefficients equal to
$0$ starting at some point, or if function diverge than all element
will be represented as machine zero starting at some point).
But we decide to use 'Stream' data type in order not to have
a branching in functions.

It's possible to use an existing library for `Stream` -- 
[Stream package](https://hackage.haskell.org/package/Stream).
However here we decide to implement our own data type for
educational purposes. However if this module will grow to a real library
the implementation likely will be changed to the one from the
common package.

> data S a = S !a (S a)

## Instances

Having a data structure we may define a set of instances.

### Functor

Series is a Functor:

> instance Functor S where
>    fmap f (S a x) = S (f a) (fmap f x)

An interesting note that $fmap ~ (f :: a \to b)$ moves
a function that is represented by a serie from $a \to a$
to $b \to b$ that may not be a desired behaviour when
$a \neq b$.

### Num

In order to use series in calculations we need to define a `Num`
instance. But befor we will introduce few helpers:

A product of scalar and series:

> (^*) :: Num a => a -> S a -> S a
> (^*) a = fmap (*a)

> (*^) :: Num a => S a -> a -> S a
> (*^) = flip (^*)

> (/^) :: Fractional a => S a -> a -> S a
> (/^) s a = fmap (/ a) s

And pointwise product for future (see Library section for szipWith implementation):

> (^*^) :: Num a => S a -> S a -> S a
> (^*^) = szipWith (*)

Now we may give a 'Num' instance:

> instance Num a => Num (S a) where
>   (S a x) + (S b y) = S (a + b) (x + y)
>   abs s = fmap abs s
>   negate s = fmap negate s
>   signum s = fmap signum s
>   (S a x) * (S b y) = S (a * b) (a ^* y +  b ^* x + S 0 (x * y))
>   fromInteger x = S (fromInteger x) 0

Here are 2 tricky parts, first one is implementation of `*` second one
is implementation of `fromInteger`. For `*` we need to think of an expansion
like of polymonial, i.e. $S a b = a + b \cdot p$. Then we can write the following:

$$
\begin{aligned}
 (S~a~x) \cdot (S~b~y) &= (a + x p) \cdot (b + y p) = \\
  &= (a b + a y p + b x p + x y p^2) = \\
  &= S~(a b)~(a y + b x + x y p)     = \\
  &= S~(a b)~(fmap~(*a)~y + fmap~(*b)~x + S~0~(x * y))
\end{aligned}
$$

There are 2 possible implementations for fromInteger:

  1. `fromInteger x = S (fromInteger x) (fromInteger x)`

  2. `fromInteger x = S (fromInteger x) 0`

For `fromInteger` we need to select an implemention such that $fromInteger 1 * a == a$
$fromInteger 0 + a = a$ $fromInteger 0 * a == 0$ for any $a$. So then we see that we
can select only the second one, otherwise properties for $1$ will not hold.

### Fractional

Now we can add a simple Fractional instance.

We say that $(S\,b\,y) = \cfrac{1}{(S\, a\, x)}$ iff $(S\, b\, y)$ is the solution of
equation $(S\,b\,y)\,\cdot\,(S\,a\,x) = 1$. So the following intance is the result of
this system of equations:

$$
\begin{aligned}
  b_0 a_0 & = 1,\\
  b_0 a_1 + b_1 a_0 & = 0,\\
  b_0 a_2 + b_1 a_1 + a_2 a_0 & = 0,\\
  \ldots
\end{aligned}
$$

These equations can be resursively solved by moving the last term from left-side
convolutions to the right side, and dividing by $(-a_0)$. Moreover the rest of
terms on the left are convolutions too, then as now series $a$ convolve with the tail
of series $b$. That fact may be used for compact of definition of recursive equations:

$$
\begin{aligned}
  b_0 & = \cfrac{1}{a_0},\\
  b_i & = \cfrac{-1}{a_0} \sum\limits_{j = 0}^{i - 1} b_{j} a_{j + 1}.
\end{aligned}
$$

> instance Fractional a => Fractional (S a) where
>   recip (S a x) = fix $ fmap (/ (-a)) . S (-1) . (* x)
>   fromRational x = S (fromRational x) 0

Formulas for composition and inversion. Only series with zeroed head can be composed
or reversed. Composition is handled according to Horner's method:

$$
\begin{aligned}
  f(x) &= \sum\limits_{i=0}^{\infty} a_i x^i =
           a_0 + x \sum\limits_{i=0}^{\infty} a_{i+1} x^i,\\
  g(x) &= \sum\limits_{j=1}^{\infty} b_j x^j =
           x \sum\limits_{j=0}^{\infty} b_{j+1} x^j, \\
  f(g(x)) &= a_0 + g(x) \sum\limits_{i=0}^{\infty} a_{i+1} g(x)^i = \\
          &= a_0 + x \left(\sum\limits_{j=0}^{\infty} b_{j+1} x^j\right)
              \left(\sum\limits_{i=0}^{\infty} a_{i+1} g(x)^i\right).
\end{aligned}
$$

The product of two series can be treated recursively, so:

> compose :: (Num a, Eq a) => S a -> S a -> S a
> compose (S a x) (S 0 y) = S a (y * compose x (S 0 y))
> compose _ _ = error "compose: Non-zero head"

Inversion can be done by the following formula:

$$
\begin{eqnarray}
  f(g(x)) & = x, \\
  a_1 g(x) + a_2 g^2(x) + \ldots & = x, \\
  g(x) (a_1 + a_2 g(x) + \ldots) & = x, \\
  b_1 x + b_2 x^2 + \ldots & = \cfrac{x}{a_1 + a_2 g(x) + \ldots}, \\
  b_1 + b_2 x + \ldots & = \cfrac{1}{a_1 + a_2 g(x) + \ldots}.
\end{eqnarray}
$$

The last equality gives the recursive rule:

> inverse :: (Fractional a, Eq a) => S a -> S a
> inverse (S 0 x) = let y = S 0 (recip $ compose x y) in y
> inverse _ = error "inverse: Non-zero head"

According to general theory elementary special functions cannot
be evaluated on the non-zero headed series. We try to handle this situaton by
breaking the series into two parts: the head and the zero-headed tail.

The head is treated by classic functions, the tail is treated by composing argument
series with classical series of elementary functions, and the combination is done by
ad-hoc formulas. The first example of the ad-hoc formula:

$$
\begin{eqnarray}
  \sin \sum\limits_{i = 0}^{\infty} a_i x^i =
       \sin (a_0 + \sum\limits_{i = 1}^{\infty} a_i x^i) =
  \sin a_0 \cdot \cos \sum\limits_{i = 1}^{\infty} a_i x^i +
       \cos a_0 \cdot \sin \sum\limits_{i = 1}^{\infty} a_i x^i.
\end{eqnarray}
$$

Here $\cos a_0$ and $\sin a_0$ are sine and cosine of scalar. And the sine/cosine
of zero-headed series can be calculated by composition of the argument with well-known
series of sine/cosine.

The second example. Let's look at the $\arcsin$, definitions:

$$
\begin{eqnarray}
  \sum\limits_{i = 0}^{\infty} b_i x^i = \arcsin \sum\limits_{i = 0}^{\infty} a_i x^i,\\
  \sin\sum\limits_{i = 0}^{\infty} b_i x^i = \sum\limits_{i = 0}^{\infty} a_i x^i.
\end{eqnarray}
$$

And solution for an ad-hoc formula is obtained as follows (using the fact $b_0 = \arcsin a_0$):

$$
\begin{eqnarray}
  \sin\sum\limits_{i = 1}^{\infty} b_i x^i =
    \sin\left(\sum\limits_{i = 0}^{\infty} b_i x^i - b_0 \right) = \\
  = \cos b_0 \cdot \sin\sum\limits_{i = 0}^{\infty} b_i x^i -
    \sin b_0 \cdot \cos\sum\limits_{i = 0}^{\infty} b_i x^i = \\
  = \sqrt{1 - a_0^2} \cdot \sum\limits_{i = 0}^{\infty} a_i x^i -
    a_0 \cdot \sqrt{1 - \left(\sum\limits_{i = 0}^{\infty} a_i x^i\right)^2}.
\end{eqnarray}
$$

The full formula is done by adding $b_0$:

$$
\begin{equation}
  \sum\limits_{i = 0}^{\infty} b_i x^i =
    \arcsin a_0 + \arcsin
       \left[
       \sqrt{1 - a_0^2} \cdot
       \sum\limits_{i = 0}^{\infty} a_i x^i -
       a_0 \cdot \sqrt{1 - \left(\sum\limits_{i = 0}^{\infty} a_i x^i\right)^2}
       \right].
\end{equation}
$$

> instance (Floating a, Eq a) => Floating (S a) where
>   pi = S pi 0
>   exp (S 0 x) = texp `compose` S 0 x
>   exp (S a x) = exp a ^* exp (S 0 x)
>   log (S 0 x) = tlog `compose` S 0 x
>   log (S a x) = S (log a) 0 + log (S 0 $ fmap (/ a) x)
>   sin (S 0 x) = tsin `compose` S 0 x
>   sin (S a x) = sin a ^* cos (S 0 x) + cos a ^* sin (S 0 x)
>   cos (S 0 x) = tcos `compose` S 0 x
>   cos (S a x) = cos a ^* cos (S 0 x) - sin a ^* sin (S 0 x)

An interesting example is sqrt, as usual we want to find $b_n$ such that:

$$ \sum\limits_{n=0}^\infty b_nx^n \cdot \sum\limits_{n=0}^\infty b_nx^n = \sum\limits_{n=0}^n a_i x^n $$

Rewrite the formula in a head-tail form, where $y$ -- is a tail of b,
and $p$ -- is a tail of incomming series:

$$ (b_0 + x * y) (b_ 0 + x * y) = a0 + x p$$

$$ b_0^2 + 2 x y + x^2 y = a0 + x p$$

by grouping elements with 0 and 1 power of $x$, we find:

$$
\begin{equation}
  \left\lbrace
  \begin{matrix}
     b_0 & = \sqrt{a_0} \\
     y & = \frac{p-y^2}{2 \sqrt{a_0}}
  \end{matrix}
  \right.
\end{equation}
$$

>   sqrt (S 0 (S 0 x)) = S 0 (sqrt x)
>   sqrt (S 0 _) = let sq = S (0 / 0) sq in S 0 sq
>   sqrt (S a x) = let sqa = sqrt a
>                      sqx = (x - S 0 (sqx * sqx)) /^ (2 * sqrt a)
>                  in S sqa sqx
>   asin (S 0 x) = tasin `compose` S 0 x
>   asin (S a x) = let S _ y = sqrt (1 - a * a) ^* (S a x) -
>                              a ^* sqrt (1 - (S a x) * (S a x))
>                  in S (asin a) 0 + asin (S 0 y)
>   acos (S 0 x) = tacos `compose` S 0 x
>   acos (S a x) = let S _ y = a ^* (S a x) -
>                              sqrt (1 - a * a) ^* sqrt (1 - (S a x) * (S a x))
>                  in S (acos a) 0 + acos (S 0 y)
>   atan (S 0 x) = tatan `compose` S 0 x
>   atan (S a x) = let S _ y = S 0 x / (1 + a ^* (S a x))
>                  in S (atan a) 0 + atan (S 0 y)
>   sinh x = (exp x - exp (-x)) /^ 2
>   cosh x = (exp x + exp (-x)) /^ 2
>   asinh x = log (x + sqrt (x * x + 1))
>   acosh x = undefined -- log (x + sqrt (x * x - 1))
>   atanh x = log ((1 + x) / (1 - x)) /^ 2


## Library

Now we can define mathematic functions on the power series. The derivative:

> diff :: Num a => S a -> S a
> diff s = fmap fromInteger (fromList [1..]) ^*^ stail s

And the integral:

> integral :: Fractional a => a -> S a -> S a
> integral c  s = S c (s ^*^ fmap (recip.fromInteger) (fromList [1..]) )


### Generic functions:

In order to inspect a stream we can introduce a helper function:

> stake :: Int -> S a -> [a]
> stake 0 _       = []
> stake n (S a s) = a:stake (n-1) s

Here all functions will be prefixed with `s` however if you write a module for working
with streams you may prefer to not add it and ask user to import module qualified.

Here is a function that builds a stream from the list (and the other way):

> fromList :: [a] -> S a
> fromList (x:xs) = S x (fromList xs) -- works only on infinite list
> 
> fromListNum :: Num a => [a] -> S a
> fromListNum [] = 0
> fromListNum (x:xs) = S x (fromListNum xs)
>
> toList :: S a -> [a]
> toList (S x xs) = x : toList xs

In order to write an usefull functions and series we will introduce a folding:

> sscan :: (a -> b -> a) -> a -> S b -> S a
> sscan f i (S x s) =
>  let k = f i x
>  in S k (sscan f k s)

Now lets introduce few functions using `sfold`. A function that will generate
a sum of the Stream, i.e. having a stream `ssum <a0:a1:a2:..> = <a0:a0+a1:a0+a1+a2:...>`

> ssum :: Num a => S a -> S a
> ssum = sscan (+) 0

Build a stream by iterating a function

> siterate :: (a -> a) -> a -> S a
> siterate f x = S x (siterate f (f x))

Build a serie of powers: $<x,x^2,x^3,...>$

> spower x = S 1 (siterate (*x) x)

Unfold

> sunfold :: (c -> (c,a)) -> c -> S a
> sunfold f k = let (k',a) = f k in S a (sunfold f k')

Because we want to implement a teylor serie we want to have a serie of factorials

> sfac :: (Enum a, Num a) => S a
> sfac = sscan (*) 1 (fromList [1..]) 

And now 1/factorials

> sdfac :: Fractional a => S a
> sdfac = fmap (\x -> 1 / (fromIntegral x)) sfac

> szipWith :: (a -> b -> c) -> S a -> S b -> S c
> szipWith f (S a x) (S b y) = S (f a b) (szipWith f x y)

This is an actual building of the Taylor serie:

> build :: Fractional a => a -> S a -> S a
> build t s = ssum $ spower t ^*^ s


> shead :: S a -> a
> shead (S a s) = a

> stail :: S a -> S a
> stail (S a s) = s

> sdropWhile :: (a -> Bool) -> S a -> S a
> sdropWhile p s@(S a xs)
>   | p a = sdropWhile p xs
>   | otherwise = s

> eps :: (Ord a, Num a) => a -> S a -> a
> eps e s = snd . shead . sdropWhile (\(x,y) -> abs (x - y) >= e)
>         $ szipWith (,) s (stail s)

## Taylor series for some analytic functions

Here we defined examples for the functions with analytically known
series at 0.

> texp :: Fractional a => S a
> texp = S 1 sdfac

> tlog :: Fractional a => S a
> tlog = let go n s = S (s / n) (go (n + 1) (-s))
>        in S 0 (go 1 1)

> tsin :: Fractional a => S a
> tsin = let s = S 0 . S 1 . S 0 . S (-1) $ s
>        in s ^*^ texp

> tcos :: Fractional a => S a
> tcos = let s = S 1 . S 0 . S (-1) . S 0 $ s
>        in s ^*^ texp

> tasin :: Fractional a => S a
> tasin = let go n = S (1 / n) (fmap (* (n / (n + 1))) . S 0 $ go (n + 2))
>         in S 0 (go 1)

> tacos :: (Eq a, Floating a) => S a
> tacos = pi / 2 - tasin

> tatan :: Fractional a => S a
> tatan = let go s n = S (s / n) . S 0 $ go (-s) (n + 2)
>         in S 0 $ go 1 1

> tsinh :: Fractional a => S a
> tsinh = let s = S 0 . S 1 . S 0 . S 1 $ s
>         in s ^*^ texp

> tcosh :: Fractional a => S a
> tcosh = let s = S 1 . S 0 . S 1 . S 0 $ s
>         in s ^*^ texp

> tasinh :: Fractional a => S a
> tasinh = let go s n = S (s / n) (fmap (* (n / (n + 1))) . S 0 $ go (-s) (n + 2))
>          in S 0 (go 1 1)

> tacosh :: Fractional a => S a
> tacosh = undefined

> tatanh :: Fractional a => S a
> tatanh = let go n = S (recip n) . S 0 $ go (n + 2)
>          in S 0 (go 1)

## TODO

Some things were not covered in this file, taylor series were not 
statically protected agains calling actions on the series build
at the different points, so we assume that every Taylor serie is
built at $0$.

Not all functions from the Floating instance were implemented.

It's possible to generalize $asin/sin$ approach for a wider class
of functions, but it was not done yet.

