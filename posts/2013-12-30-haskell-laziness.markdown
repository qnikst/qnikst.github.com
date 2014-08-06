----
title: Playing with types: type level numbers
author: Alexander Vershilov
tags: haskell typefun typenats
data: 2013-12-30
----


> As always I'm terribly sorry for my bad english, but writing
> the posts and having a feedback is the only reasonable way 
> to improve it.

This post describes an easy exercise that gives a feeling of 
using typelevel numbers.

If you want to know more about typelevel literals and friends
you can take a look at the [following post](http://ponies.io/posts/2014-07-30-typelits.html)
it describes the theory much better than I can.

Intead of using type level Strings we will address a numbers.
In this post I want to create a datatype that will contain a 
number that is known statically (at compilation time) and that
have a way to use that number in runtime.

The first part of the problem (having a number) in a type may
be interesting if you want to distiguish between different types.
