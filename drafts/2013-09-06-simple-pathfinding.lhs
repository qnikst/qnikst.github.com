---
author: Alexander Vershilov
date: 2013-09-06
title: What I have learned today: comonad
mathjax: on
license: cc-by
---

Lets start from writing a types, we have

> type Loc  = Int  -- index of location
> type Dist = Int -- distance
> type To   = Loc -- target location
> type From = Loc -- from location

So solve such a task we can create a stream (lazy list) of possible solutions
and filter the solutions we are interested in. So generic solution
will be:

> solve0 = filter isSolution . build
>   where isSolution = undefined
>         build = undefined

Common scheme is quite more complicated but we will not discuss it. Interested
reader can refer to 'Pearls of the functional algorithm design' book by R. Bird.

To create a stream we may use 'Data.List.unfoldr' from base, it's type is
unfoldr :: (b -> Maybe (a,b)) -> b -> [a]. It takes seen 'b' and return element
plus next seed.

> solve1 = filter isSolution . unfoldr go 
>   where isSolution = undefined
>         go = undefined

We will use a basic search algorithm:

  1. we will take all destination from our current point;
  2. add a distance to current point from start to distance from current point
     to destination;
  3. filter our destinations that we have already visited;
  4. sort destinations by distance;
  5. add destinations to list of the frontier;

So our seed should contain all data we need to perform this algorithm:
  1. current location
  2. distance to current location
  3. list of possible location (frontier)
  4. list of visited locations

> solve2 = filter isSolution . unfoldr go
>   where isSolution = undefined
>         go (loc,dist,visited,frontier) =
>             

