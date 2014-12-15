---
author: Vershilov A.V.
title: Compositional methods for numerical ODE integrators
date: 2014-08-18
license: by
mathjax: on
tags: haskell, math
---

We show a way to improve properties of a ODE integrators, by
introducing a composition of the methods with a different steps.

<!--more-->

This post is based on the work of the Ernst Harrier [[1](http://www.springer.com/mathematics/computational+science+%26+engineering/book/978-3-540-30663-4)] and doesn't
contain any additional research work.

Here we want to solve a system of equations that can we written
as:

$$ \dot{y} = f(y). $$

In order to solve it we are introducing a mapping from an old
state at $t0$ to a new one at $t_1 = t_0 + dt$:

$$\Phi_h: y_{n} \rightarrow  y_{n+1}$$

In order to increase the order of the solution while preserving
some desirable properties of the base method we may prepare a
compositional method:

$$\Psi_h = \Phi_{\gamma_1h} \circ \ldots \circ \Phi_{\gamma_nh},$$

where $\gamma_i$ is a coefficient from $\mathbb R$. This approach 
was studied by Suzuki, Yoshina, McLackcan in 1990th. Here we compose
a base method at a different points in time.

We have a theorem about this approach to compositional methods.

*Theorem*

> Let $\Phi_h$ be a one-step method of order $p$. If
> 
$$
\begin{eqnarray}
   \gamma_1 + \ldots + \gamma_s = 1 \\
   \gamma_1^{p+1} + \ldots + \gamma_s^{p+1} = 0 \\
\end{eqnarray}
$$ 
>then the compositional method $\Psi_h$ is at least of the order $p+1$.


This gives theorem gives us a nice way to improve properties of the existing
method. The question now is how to find a good coefficients $\gamma_i$.

The first notice is that equations does not have a real solution for the odd $p$,
so we can improve only solutions with even $p$.

The smallest number $s$ where a solution in reals exists is $3$.
And coefficients are defined as:

$$ \gamma_1 = \gamma_3 = \frac{1}{2 - 2^{\frac{1}{p+1}}} $$

$$ \gamma_2 = - \frac{2^{\frac{1}{p+1}}}{2 - 2^{\frac{1}{p+1}}} $$

This method is called tripple jump. Lets check how does it work.

At first we will introduce a coefficients

~~~haskell
g1 :: Int -> Double
g1 p =  1 / (2 - 2**(1/(fromIntegral p+1)))

g2 :: Int -> Double
g2 p = - 2**(1/(fromIntegral p+1)) / ( 2 - 2**(1/(fromIntegral p+1)))

g3 :: Int -> Double
g3 = g1
~~~~~~~~~~~~~~~~~

Having a method of an order 2 (for example standard Runge-Kutta method) we may
use a composition a points defined by $\gamma_i$ with $p=2$.
Let step be a $dt = 1$ for simplicity.

~~~~haskell
> t :: Int -> Double -> [Double]
> t p dt = map (*dt) [g1 p, g2 p, g3 p]

*Main> t 2 1
[1.3512071919596578,-1.7024143839193153,1.3512071919596578]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We have 3 points. If we will take a compositional method $\Psi$ in the points we
got then we will have a method of order $3$. However if you 
method is symmetric then it's order is $4$ and we can apply a tripple jump
once again to our composed method.

~~~~~~~haskell
> ut :: Int -> [Double] -> [Double]
> ut p xs = xs >>= (\x -> t (p+2) x)

*Main> ut 4 (t 2 1)
[1.5081944151591316,-1.665181638358605,1.5081944151591316,-1.900205890992877
,2.097997398066439,-1.900205890992877,1.5081944151591316,-1.665181638358605,1.5081944151591316]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a coefficients for a compositional method of order $5$, ($6$ due
to symmetry. Applying tripple jump nce again:

~~~~~~~haskell
*Main> ut 6 \$ ut 4 (t 2 1)
[1.639448210847001,-1.77070200653487,1.639448210847001,-1.8100975778074668
,1.955013517256328,-1.8100975778074668,1.639448210847001,-1.77070200653487
,1.639448210847001,-2.0655753110586246,2.2309447311243717,-2.0655753110586246
,2.2805800406433505,-2.4631626832202618,2.2805800406433505,-2.0655753110586246
,2.2309447311243717,-2.0655753110586246,1.639448210847001,-1.77070200653487
,1.639448210847001,-1.8100975778074668,1.955013517256328,-1.8100975778074668
,1.639448210847001,-1.77070200653487,1.639448210847001]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

this is a compositional method of order 8. 

To see a places where function will be evaluated we can use:


~~~~~~~~haskell
*Main> scanl (+) 0 $ ut 6 $ ut 4 (t 2 1)
[0.0,1.639448210847001,-0.131253795687869,1.508194415159132,-0.30190316264833483
,1.6531103546079933,-0.15698722319947356,1.4824609876475274,-0.28824101888734255
,1.3512071919596584,-0.7143681190989661,1.5165766120254056,-0.548998699033219
,1.7315813416101316,-0.7315813416101302,1.5489986990332203,-0.5165766120254043
,1.7143681190989675,-0.3512071919596571,1.2882410188873439,-0.4824609876475261
,1.156987223199475,-0.6531103546079919,1.3019031626483362,-0.5081944151591307
,1.1312537956878703,-0.6394482108469997,1.0000000000000013]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This way we may obtain a method of any order by the price of a terrible zig-zag
of the step points.

Another approach to a compositional method is using Suzuki`s Fractals.

The same schema exists for Suzuki`s Fractals, however how we have a diffierent
coefficients:

$$
\begin{eqnarray}
  \gamma_1 = \gamma_2 = \gamma_4 = \gamma_5 = \frac{1}{4-4^{\frac{1}{p+1}}} \\
  \gamma_3 = - \frac{4^{\frac{1}{p+1}}}{4-4^{\frac{1}{p+1}}}
\end{eqnarray}
$$

However $t$ and $ut$ methods looks quite ugly and we may want to improve
this situation.

~~~haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeLits
import GHC.Exts (Constraint)
import Data.Proxy

-- Coefficients
g1 :: Integer -> Double
g1 p =  1 / (2 - 2**(1/(fromIntegral p+1)))

g2 :: Integer -> Double
g2 p = - 2**(1/(fromIntegral p+1)) / ( 2 - 2**(1/(fromIntegral p+1)))

g3 :: Integer -> Double
g3 = g1

-- Description of the method
data RK2 = RK2

-- Description of the method order
type family Order a :: Nat
type instance Order RK2 = 2

-- Description of symmetric properties of the method
type family IsSymmetric a :: Constraint
type instance IsSymmetric RK2 = ()

-- One level composition
buildComposePoints :: forall p . KnownNat (Order p)
                   => p -> Double -> [Double]
buildComposePoints p dt = map (*dt) [g1 o, g2 o, g3 o]
  where
    o = natVal (Proxy :: Proxy (Order p))

-- Composition for the symmetric method
buildComposePointsSym :: forall p n . (UpdateCompose (Order p + 2) n, IsSymmetric p, KnownNat (Order p), KnownNat n)
                      => p -> Proxy n -> Double -> [Double]
buildComposePointsSym p pn dt = update (Proxy :: Proxy ((Order p) + 2)) pn (buildComposePoints p dt)

class UpdateCompose (k :: Nat) (v::Nat) where
  update :: Proxy k -> Proxy v -> [Double] -> [Double]

class UpdateComposeCase (leq :: Bool) (k :: Nat) (v :: Nat) where
  updateCase :: Proxy leq -> Proxy k -> Proxy v -> [Double] -> [Double]

instance UpdateComposeCase (k <=? v) k v => UpdateCompose k v where
  update = updateCase (Proxy :: Proxy (k <=? v))

instance UpdateComposeCase False k v where
  updateCase _ _ _ = id
  
instance (KnownNat k, UpdateCompose (k+2) v) => UpdateComposeCase True k v where
  updateCase _ k v ds = update (plus2 k) v (ds >>= \x -> map (*x) [g1 o, g2 o, g3 o])
    where
      o = natVal k
      plus2 :: Proxy n -> Proxy (n+2)
      plus2 _ = Proxy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
