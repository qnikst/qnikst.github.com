----
title:  Runtime based caching
date: 2019-12-30
author: Alexander Vershilov
tag: Haskell, work
----

Today I want to continue describing how we have made our services fast enough.
In the contest service, one obvious bottleneck is the database.
When many users send requests we have to generate lots of connections and
perform many queries. The obvious solution here is to introduce a caching mechanism.
In this post I'm going to describe some solutions available in the Haskell
ecosystem and our solution which is quite interesting.

Firstly we define options we need:

   * We need a caching mechanism that saves us from queries to the database.
   * A cache should be memory bound so our application does not consume all available memory.

Besides that we want to be able to set up sound strategies for cache eviction 
and we will discuss that before showing our solution.

# Option one: use LRU-cache. 

The simplest option is to take some [LRU cache](https://en.wikipedia.org/wiki/Cache_replacement_policies#Least_recently_used_(LRU)). This approach keeps memory bound by allowing only fixed amount of items to exist in cache. And it provides a strategy for increasing hit-rate by keeping least recently used items in cache. 

The best Haskell package for the LRU cache I've found so far is [lrucaching](https://hackage.haskell.org/package/lrucaching). This package is based on the [psqueues](https://hackage.haskell.org/package/psqueues) package that provides the fastest immutable priority queues in the Haskell ecosystem.
A pseudo-code for this could look like:

```Haskell
   result <- atomicModifyIORef' ref $ \cache ->  {-1-}
     case lookup key cache of          {- 2-}
       Nothing              -> (cache, Left key)
       Just (value, cache') -> (cache', Right value)
   case result of
     Left key' -> do
       result <- performQuery key'
       atomicModifyIORef' ref $ \cache ->
         (insert key value cache, ())
       pure result
    Just result -> pure result
```
Here we first check if the value is in the cache or it's not.
We use an `atomicModifyIORef'` call that is a `CAS` operation that is quite fast
but we may run several retries under contention.
Then we perform a query and save a result of a computation in the cache if needed,
or just return a cached result otherwise.

The solution is nice and simple. We use it in several places in our codebase.
However, we have a problem with this solution.
If many threads come for the same key then all the requests will not find the
key and execute a query against the database. So this approach does not save us from the
first requests burst that is the most dangerous for our service. So we want
to solve the problem of such bursts and we want to increase our chances that if two
requests come for the same key simultaneously then only one request to the database is made. 

The simplest but not affordable solution is to introduce a critical section so only one request is run at a time:

```Haskell
withMVar lock $ \_ -> do
   atomicallyModifyIORef ref $ ...
````

This solution is too coarse; we still want requests for the different thread to be run simultaneously. We can achieve that by keeping a lock in the cache. Still, if we go this way solution exceeds the complexity budget quite fast: you'll need STM solution with explicit locking and careful exception handing. You can try implementing that yourself.

The interesting thing is that the GHC runtime system already provides the tooling that is enough to build such a cache without an explicit lock. When a thunk gets evaluated all other threads accessing that thunk block on evaluation and automatically get a result once it's evaluated. Now let's check our solution.

# Option two: Haskell runtime based solution.

Before describing the solution let's talk a bit more on the cache eviction. In the early stages of our system we have decided that cached values may be updated but it's ok to keep value in cache for a limited time. So LRU cache doesn't work here as naive variant does not provide such a guarantee: a value may live in cache forever. So instead we keep unlimited amount of values but remove values that are too old. We are using the psqueues package still.

So the cache itself provides the following interface:

```Haskell
data Handle key result = Handle
  { requestOrInternal
    :: POSIXTime -- ^ current time
    -> key -- ^ key
    -> (key -> IO result) -- ^ function to get the value
    -> IO result
  , ...
  }
```

The meaning of this is the following: we provide a current time value, a key, and a function that generates a value in case if the key is not found. And this function returns a result (or throws an exception).
Handle provides us with a way to change the actual implementation without changing the interface and code that uses it, you may read more about that approach in the following posts [1](https://www.schoolofhaskell.com/user/meiersi/the-service-pattern), [2](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html).


Now, to the actual implementation. Let's introduce that line by line:

```Haskell
new :: IO (Handle a b)
new = do
 ref <- newIORef PSQ.empty -- Create new priority queue.
 pure $ Handle 
 { requestOrInternal = \current_time key f -> mdo 
   ...
```
`mdo` - provides [recursive do notation](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#the-mdo-notation). It allows us to refer to the values that we will get in the future. 

Now we need to read the cache to see if there is a value there:
```Haskell
  m_result <-
    atomicModifyIORef ref
    $ swap
    . PSQ.alter
        (\case
          Just (p, ~v)
            | p >= current_time ^-^ configLongestAge -> {- 1 -}
              (Just v, Just (current_time, v))
          _ -> (Nothing, Just (tm, Lazy eresult)) {-2-}
        )
        key
```
We update a value for the key and get a result using the following rules:

  * `{-1-}` in case if there is a value and it lived for not more than maximum allowed age then we keep the value untouched and return that value.
  * `{-2-}` otherwise we return nothing from the cache and store the result of our future call in the cache (!).

So basically we have stored a result that we don't even have at the moment, as we have not run the query still. It may sound mindblowing but it's perfectly fine in a lazy language.

Note lazy matching of the value `~v` on the line `{-1-}`; it means that we don't try to inspect the value and return whatever is there. It seems that this protection is not required but it's better to be safe than sorry.

Another thing is that we wrap our result in a `Lazy` data structure:

```Haskell
data Lazy a = Lazy { getLazy :: a}
```

This way `WHNF` of the value doesn't evaluate result itself, otherwise, we'd get a `<loop>` when storing result (as we don't have the result yet and our thread would need the result at hands to finish `alter` action).

Now we can analyze the result and perform query if needed (skipping some logging):

```Haskell
  eresult <- try $ maybe
    (f k) {-1-}
    (\r -> do
      evaluate (getLazy r) >>= \case {-2-}
        Left s -> throwIO s
        Right x -> pure x
    )
    m_result
```

We analyze the result, as you remember in a case if we have `Nothing` there we should perfom our request, and we do that on line `{-1-}`. We bind the result of our request to the `result` name (this is exactly the value we have already put in the cache). It keeps either exception value or the result.
Otherwise, there is a result (or a thread working on generating that result). We need to force that `(evaluate (getLazy r))`. In a case when the value is already evaluated, we get the result immediately. Otherwise, we block on evaluation, and the Haskell runtime handles it.
We have 2 options afterwards, either the query results in an exception and in that case we rethrow it, or we get a result.

Now we need to do some cleanup: we do not want to keep exception result if it gets returned so we clean the cache in this case:

```haskell
  result <- case eresult of
    Left (s::SomeException) -> do
      atomicModifyIORef' ref $ \v -> (PSQ.delete k v,  ())
      throwIO s
    Right x -> pure x
```

We must keep value with an exception in the cache first: this way all the threads that made the same request while our thread is performing request will raise an exception.

The last step is to try to clean the oldest value. There may be better strategies for cleaning but the following strategy is straightforward and it works:

```Haskell
  atomicModifyIORef' ref $ swap . PSQ.alterMin
    (\case
      Nothing -> ((), Nothing)
      Just (kk, p, v)
        | p < tm ^-^ configLongestAge -> ((), Nothing)
        | otherwise -> ((), Just (kk, p, v))
    )
  pure result
```

This solution worked just fine for 1.5 years under heavy usage and we have experienced 2 bugs in this piece of code:

   * One found during early testing stage (it even didn't hit a repository branch): the requirement to use the `Lazy` wrapper
   * Another one found after a year of usage in production when an exception happened in request f. Previously we kept `Lazy result` in the cache instead of `Lazy (Either SomeException result)`. As a result, in case of an exception in f nobody can populate the value in the cache. And other threads wait forever on update. This unfortunate event has hit 2 users :(.

