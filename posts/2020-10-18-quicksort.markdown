----
title: Quicksort in Haskell
date: 2020-10-18
author: Alexander Vershilov
tag: haskell, rewriting-rules, fun
----

Today we will write a proof of concept of quicksort in Haskell. Quicksort is a code
that is usually shown as an example of the elegant Haskell code. You may see that
code in the lots of presentations, one of the first implementations was
implemented in [SASL](https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#SASL)
in Nov 1983:

```
DEF || this rather nice solution is due to Silvio Meira
sort () = ()
sort (a : x) = sort {b ← x; b ≤ a}++ a:sort { b ← x; b>a}
```

The code is very simple:

```Haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = left ++ [x] ++ right where
  left = quicksort (filter (<=x) xs)
  right = quicksort (filter (>x) xs)
```

Unfortunately, this is not a real quicksort as it doesn't run in place killing all the
idea. And we want to fix that! 

This code will be a proof of concept for two reasons:

  1. we will not write a real quicksort implementation, you can always find 
     how to do that in the other blog posts for example [this one](https://mmhaskell.com/blog/2019/5/13/quicksort-with-haskell)
  2. We will not try to capture all the cases and handle only the basic one.

So what we can do? The core idea is to learn the compiler to catch the code
and write real quicksort instead. But it would be very hard to solve the problem
in general, so we want to teach the compiler to find pattern above and substitute
better implementation instead of that code.

For the better implementation we will use:

```Haskell

import Debug.Trace (trace)
import Data.List (sort)

-- | implementation of the quicksort
good_sort :: (Ord a) => a -> [a] -> [a]
good_sort a as = trace "good" $ sort (a:as)
```

[`Debug.Trace.trace`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Debug-Trace.html#v:trace) is a debug function. It addition to calculating a pure value it prints a message to
stdout. For our purpose, it will be enough, if we see "good" in stdout than our task 
is solved. In a real solution instead of `Data.List.sort` we will have
`Data.Vector.toList . Data.Vector.modify quicksort . Data.Vector.fromList` with real
quicksort implementation.

So how we will teach compiler to make a good substitution without solving very complex
optimization task? There are two ways forward:

  1. source plugin that will analyze code, find the relevant part and do a substitution.
Someday I'll try that.
  2. Use [rewriting rules mechanism](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rewrite_rules.html). And we will use it.

Rewriting rules is a mechanism that tells GHC how to rewrite a certain pattern in the code.
This is a very powerful mechanism that is used in [deforestaion (pdf)](https://ccsweb.lanl.gov/~kei/deforestation-short-cut.pdf)  and [stream techniques](http://fun.cs.tufts.edu/stream-fusion.pdf),
techniques that allow to remove intermediate data structures and allows programs to run
without too many allocations. Unfortunately rewriting rules are very fragile it's very hard
to tell how exactly the rules will interact and it's easy to make program much execution
much worse. Fortunately, you can hit that only in very special cases with `vector`s or `text`
and it's quite easy to workaround the problem. Anyway, we will try this path.


```haskell
{-# RULES
"quicksort/left" [2] forall x xs . filter (<=x) xs = quick_left x xs

"quicksort/right" [2] forall x xs . filter (>x) xs = quick_right x xs

"quicksort" [2] forall x xs .
  quicksort (quick_left x xs) .<> [x] .<> quicksort (quick_right x xs)
     = good_sort x xs
 #-}
```

Rule consist of the following parts:

  * Name `"quicksort/left"`, name is shown in debug mode `-ddump-file-firings` when rule
     fires (applied).
  * Phase `[2]`. Tells when the rule can fire. A number `N` means that it can fire starting
     from phase `N` and below. There are phases 2,1,0 in GHC. If you see `[~N]` it means
     that the rule can fire before phase `N` occur.
  * Variables `forall x xs` we use, we can add type signatures for such variables,
    in that case, the rule is applied only if types match.
  * LHS Pattern `filter (<=x) xs` — if the compiler sees this pattern it rewrites it.
  * RHS `quick_left x xs` — the code pattern is rewritten to.


So above you see the idea, we introduce 3 patterns we rewrite, 2 clauses we rewrite
into functions quick_left and quick_right and the final one that rewrites entire sort.

You can see that we use `.<>` here, we will come to that a bit later, first let's check
what are `quick_left` and `quick_right` functions:
   
```haskell
quick_left :: (Ord a) => a -> [a] -> [a]
{-# NOINLINE [~1] quick_left #-}
quick_left !x !xs = filter (<=x) xs
```

`quick_left` is just a wrapper over the function we rewrite, it's not inlinable, so it
will survive on phase 2. And it can be caught by the "quicksort" rule. If it was not
caught it means it was not a quicksort, so it will be inlined on phase 1, leaving
the opportunity to the normal GHC rules to optimize the code.

So we are trying to optimize code, but if we fail we leave that as-it-was. The
code will not be the same as GHC would optimize it as some rules will not fire

```haskell
"filter"     [~1] forall p xs.
  filter p xs = build (\c n -> foldr (filterFB c p) n xs)
```

after our one, so ideally we should have that on the right hand side, but there are some
problems with it. For example `filterFB` is not exposed by GHC.

Before going to (`.<>`) operator let's see how we can debug rules, we can do that using
GHC options:

  * `-ddump-rule-firings` — emits a message when the rule fires
  * `-ddump-simpl-iterations` — writes how the core was optimized
  * `-ddump-to-file` — write debug information to file

When you compile GHC dumps warnings:

```
3.hs:6:1: warning: [-Winline-rule-shadowing]
    Rule "quicksort/left" may never fire
      because rule "Class op <=" for ‘<=’ might fire first
    Probable fix: add phase [n] or [~n] to the competing rule
  |
6 | "quicksort/left" [2] forall x xs . filter (<=x) xs = quick_left x xs
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

It tells that another rule may change the definition of `<=x` so this rule will not
fire. Unfortunately such rules live on phase 2, and there is no way to override
it, but you can use [a trick](https://gitlab.haskell.org/ghc/ghc/-/issues/4397) and
introduce an additional function with `{-# NOINLINE [~1] #-}` and write a rule for that.
And with `(<>)` we can to do exactly that:

```haskell
(.<>) :: [a] -> [a] -> [a]
{-# NOINLINE (.<>) #-}
(.<>) = (<>)
```

It happens because GHC introduces many rules that convert list operations to
`foldr/build`. I think it's possible to write a better rule that will be able to
rewrite a result of the rules applications, but I failed to do that.

So we change `<>` to `.<>` and now we can run the program:

```
ghc 3.hs -ddump-rule-firings \
  -O \
  -ddump-simpl-iterations \
  -fforce-recomp \
  -ddump-to-file \
  -ddump-simpl \
  -dcore-lint && ./3
Linking 3 ...
good
[1,2,3]
```

I've used `dcore-lint` here because those rules lead to a segfault but that
happened on `ghc-8.8.3` and everything works fine on later versions.
