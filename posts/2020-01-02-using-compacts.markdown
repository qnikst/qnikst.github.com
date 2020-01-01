----
title: Using compact regions
date: 2020-01-02
author: Alexander Vershilov
tag: Haskell, work
----

Today I'd like to talk more about the tricks we've used in our codebase. 
I'd like to speak about compact regions. There will be not much code and numbers, just general thoughts.
In our contests application, we need additional restrictions:

> there are several sites (places usually schools) where people can pass the contest, and for each of them access can be done only from restricted set of networks.

So we needed a built-in IP filtering app, such that for a given contest may check whether the user can access it.
After the competition has finished, the user may have access to his results from any IP. And there can
be concurrent restricted and unrestricted contests runnning at the same time.

There are a few additional constraints:

  * Configuration should be updateable, as sometimes the site doesn't know all it's IPs.
  * Sometimes some people can pass contest from the other site, for example at the moment of the competition they moved to some other school or training.
  * The solution should not harm contest without the checks and general properties.
  * The check should not access the database.

We need the last constraint as additional protection from the DDOS attacks: we don't do any work if IP is not allowed and do not consume system resources.
Actually it was the first internal reason for the IP filtering, but the feature was very helpful
for the organizers, so they decided to use that for the main events.

Leaving aside technical details aside, we may think that for each user we can map his login on contest id,
and id of the site without access to the database. So basically we need to write a function:
```
check :: ContestId -> SchoolId -> IP4 -> Bool
```
That will check if user has an access. Note missing `IO` or any other context here.

The simplest solution is to have a

`HashMap (ContestId, SchoolId) [Net4Addr]`

or

`HashMap ContestId (HashMap SchoolId) [Net4Addr]`.

The latter one allows faster-path for the case if the contest is not filtered.

Is there any problem with this case? There is. The hashmap structure is very "branchy" tree,
and if it's quite big, GC will have hard time evaluating it. It may not be a big problem: if the tree changes rarely, it goes to the older generation and will affect major GC only. And in one project I had an experience with keeping
large (from half to serveral Gb tries in memory). However, it may still negatively
affect the performance of the service, and we want to have better story especially if it's cheap.

What other languages do in this case? There are several ways forward use some in-memory db,
or external cache like redis, both solutions provides much more functionality than needed for
this use-case. Another solution is to use off-heap data structures. In this case the
data-structure does not affect GC. This solution is possible in Haskell, and it does solve the problem,
bug such solution may be complicated, and we lose the ability to use first-class language features.

We want something better. And there is a solution: compact regions. Interested reader may check

  * compact structure Edward Z. Yang, Giovanni Campagna, Ömer Ağacan, Ahmed El-Hassany, Abhishek Kulkarni, Ryan Newton. "*Efficient communication and Collection with Compact Normal Forms*". In Proceedings of the 20th ACM SIGPLAN International Conference on Functional Programming. September 2015. http://ezyang.com/compact.html
  * [compact](http://hackage.haskell.org/package/compact) on hackage.

The compact region is a region that contains a Haskell structure inside, but it is stored in the contiguous blocks of memory, it doesn't have any references outside of the region. So it can be seen as a single object for the garbage collector. And it doesn't affect the GC.
Programmer may still access stored value and use all the haskell features when working with it.

The simplest solution may be as simple as:

```haskell
mkCheck
  :: IO (HashMap ContestId (HashMap SchoolId (Vector Net4Addr)))
  -> IO (Handle, UpdateHandle)
mkCheck mkCache = do
  ref <- newIORef =<< compact mkCache                          {- 1 -}
  let hdl = Handle { lookupSchool = \cid key ->
        runExceptT $ do
          storage <- liftIO $ getCompact $ readIORef ref       {- 2 -}
          c_storage <- HM.lookup cid storage ?! NoRestriction  {- 3 -}
          V.toList <$> HM.lookup key c_storage ?! Missing      {- 4 -}
   pure (hdl
    , UpdateHandle $ mkCache >>= compact >>= writeIORef ref)   {- 5 -}
```

Here we create a cache with update function. We take a cache population functio as a paremeter.
Then we create a cache `{- 1 -}` and create compact region out of that. We store the region in
`IORef` basic mutable variable with atomic CAS updates.
When we read the value `{-2-}` we get it our of IORef and get it from the compact and can work
with it as with any other Haskell value (steps `{-3,4-}`).
On the like `{-5-}` we return an update function that builds new value of the cache when is called.

So basically the only lines that were added to the naive algorithm are `1,2,5`, the rest of the
algorithm remains unchanged.

Note. There may be other architectural choices how to provide an access to the API and to it's
updates, but I doubt they are very solution depenent, so I'd like to avoid API discussion.

So now let's discuss this solution. During first tests on data that we have gathered from the
previous contests, the initial hashmap had the size of 177696b (reported by ghc-datasize).
That is a tiny number, but the numbers were for the single contests, and we know it will grow.
After compacting it was 98304b only (repored by `compactSize` function). Isn't it quite nice, especially
we when have that for free? The rest of the algorithm remains unchanged: the only difference is
a call to `compact` function during the update and `getCompact` at the beginning of the lookup.

Are there any other costs? There are:
during the update, we update the full structure even in the case we could update it partially.
There are more elaborated solutions that can avoid this problem. But we decided to run the nex
event using the current one: we just set update function to run once per 10 minutes.
And everything went fine, except that 10 minutes was two high value to wait when you add changes.

After examining the feedback and results of the contest we have decided to decrease update time
to one minute, and make cache structure more complex:

   * introduce network pools that are allowed for all sites in the contest. It makes sense as many schools work via shared proxy servers.
   * Store configuration per site, so when we add the site to the contest, we add that with configured networks.
   * Have an overlay per contest, per site.

After all those changes, it was not feasible to build a new structure each minute, especially
because it's known that most of the time structure does not change. Compact regions provide a
way to add structure to the region:
```
compactAdd :: Compact b -> a -> IO (Compact a)
```
this function takes compact region with reference to the value of type b, adds the value of type
`a`, and returns a reference to value `a` in the region. So if we want to update a region we can write:
```
compactAdd b (update (getCompact b))
```

If a structure update keeps old parts of the structure untouched as much as possible,
then we will store only updated parts. And for most of the persistent immutable structures (e.g. `HashMap`),
we have that already. 

But if we go this way we will never free old parts of the structures that are no longer used
and introduce memory leak. To avoid that problem, we need to add some strategy when to rebuild
the structure from scratch. So
we will call `compactAdd` often and sometimes call compact.
I've decided to keep the following policy:

> "rebuild the structure if the size of the value is twice as big as it was when the initial version was built".

```haskell
c <- compact =<< makeCache
ref <- newIORef c
old_size <- newIORef =<< compact c {-1-}
let update = do
      prev_compact <- readIORef ref
      current_size <- compactSize prev_compact
      prev_size <- readIORef old_size
      let inplace = prev_size * 2 > current_size {-2-}
      ... new_cache
      if inplace
      then compactAdd prev_compact new_cache >>= writeIORef ref {-3-}
      else do
        z <- compact new_cache  {-4-}
        new_size <- compactSize z  {-5-}
        writeIORef old_size new_size 
        setGauge metric_size $ realToFrac new_size  {-6-}
        writeIORef ref z
```
On the line `{-1-}` we store initial size in addition to the reference to cache itself.

On the line `{-2-}` we apply our strategy and decide if we want to update inplace or build a new compact region.

On the line `{-3-}` we store update to the structure in the current region

On the lines `{-4,5-}` we rebuld a new cache and on the line `{-6-}` we store the size to the metrics, so we can check if everything goes well or not.

But it's not everything we need to check how updates take place, assume you have `HashMap`.
You update a value at the given key with `id` function, you'll get the equal value, but
from the low-level perspective you'll get a new structure, and it matters when
you'll store that value to compact. So we track if there were updates and if not we
explicitly return the same structure and do not update reference at all.

With all the changes, the structure works quite nice. At some point, I'd like to put that as a library.
Unfortunately, I'm not sure if I know how to generalize from the project-specific parts.

