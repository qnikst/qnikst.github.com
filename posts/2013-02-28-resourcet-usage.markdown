----
author: Alexander Vershilov
date: 2013-02-28
title: Resourcet usage
tags: haskell, resourcet
license: by-nc-sa
----

This post describes an interesting resourcet usecase that may be usefull due to the
lack of liner-types support in haskell.


Lets look at the next problem. Assuming we have got a some sort of transaction
mechanism that receives data and then perform transaction, but sometimes it may
call callback to users code (originally it's about [2pc](http://github.com/qnikst/2pc)
library).

So we have method:

```
withWH0 :: (Binary b) -> ByteString -> (b -> IO Bool) -> IO ()
withWH0 = undefined
```

Where Bool is result of transaction i.e. either we accept or decline transaction.

This variant is not good as callback is synchronous, we will not be able to run
next transaction untill callback is finished. And it will be a bottleneck for the code.

There can be an interesting solution:
  
> one can provide an trasaction handler, i.e. a resource that for communication of the 
> user code and library, this resource should be either accepted or declined. 
> If that resource is 'alive' it means that user is working on transaction. If resource
> is not touched by user code a default action should be run, before resource will be 
> cleared.

The problem that we should somehow guarantee that user will free resource. One solution 
use linear types, but we don't have them (or at least it's too difficult for me and I 
don't know how to do it). Another option is to use some kind of region library: one option
is [regions](http://hackage.haskell.org/package/regions) originaly proposed by [Oleg](http://okmij.org/ftp/Haskell/regions.html#light-weight)
but this package doesn't work in recent ghc`s due some unsolved bugs, another option
is [resourcet](http://hackage.haskell.org/package/resourcet) package written by Michael 
Snoyman, this package doesn't give so much guarantees as regions but at least it works.
Resourcet package introduce ResourceT monad that forms a block where all resources that
were registered in that blog will be closed as soon as compulation would left a block 
(there is an api for an early close but will not look at it now).

So we can guarantee that:

  1. every resource registered in the block will be freed
  2. resources freeing will be determinated in time/
  3. resource will be freed only once  (second call to release is noop)

Here are minimal definition for what we need:


```
data TH = TH -- ^ transaction handler

-- | accept transaction (send message via network)
accept = undefined  
-- | decline transaction (send message via network)
decline = undefined
-- | real release (synonym for decline)
thRelease = undefined

withWH1 d cb = runResourceT $ do
  let d' = decode d
  th <- accure create thRelease
  cb th d'
```
Now we create a ResourceT region, register resource there and then call user code, our
code is not as safe as resourcet release as user can call accept more that once, so
we need to check if it was called.

The best thing that now client can use `resourceForkIO` to make withWH1 asynchronous.

But there is another issue: how user can queue some part of the messages?

TBD make nice picture

There is a solution: `unprotect` function. This function allowes you to degerister
resource in the current resourcet block and then register it in another block, or register new
release action.

Currently resourcet package lacks helpers that allowes to move resource to some other place, so
one need to create that functions himself. Here are some advices how one can do it safely.

You need a structure that has the following interface:

  * put :: s -> (a, IO ()) -> m ()  -- put resource into storage
  * get :: s -> m (ReleaseKey a)    -- get resource and register it in current process

Put and get should be atomic and mask exceptions, moreover if you can't guarantee that message
will be immidiatelly read by another process you should register datastructure in some higher
level ResourceT block, this way you'll guarantee that no resources will be left in store unfreed.

Here are some basic examples of such datastructures:

(this code is not well tested as I've used more complicated datastructure for such purpose, that
have an additional API, but its not usefull for common case)

At first let's write an `InstantMVar`, this structure allow you to send message into other process,
where it will be immideatelly read, so we don't need to store that structure in 'global' resource 
block

```
data InstanceMVar a = InstanceMVar (MVar (a,IO ())) (MVar ())

newMVar = InstanceMVar <$> newEmptyMVar <*> newEmptyMVar

put (InstanceMVar a b) (key, x) = mask_ $ unprotect key >>= putMVar a . ((,) x) >> takeMVar b

get (InstanceMVar a b) = mask_ $ takeMVar a >>= \(x, r) -> register r >> putMVar b () >> return x
```
This code will guarantee that resource will be read, however it may lock.

Let's write a `DelayedMVar` this structure will not block on write, but should be registered
in global resourcet block

```
newtype DelayedMVar a = DelayedMVar (MVar (a,IO ()))

newDelayedMVar = allocate (DelayedMVar <$> newEmptyMVar) (releaseDelayedMVar)

releaseDelayedMVar (DelayedMVar v) = maybe (return ()) (snd) =<< tryReadMVar v

put (DelayedMVar v) (key, x) = mask_ $ unprotect key >>= putMVar a . ((,) x)

get (DelayedMVar v) = mask_ $ takeMVar a >>= \(x, r) -> register r >> return x
```

Same way one can write Channel or STM Channel, however channel is not fully safe unless chan 
is closable.

```
newtype SChan a = SChan (SChan (a,IO ()))

newSChan = allocate (SChan <$> newChan) releaseChan

releaseChan (ChanMVar v) = go =<< tryReadChan v
  where go Nothing = return ()
        go (Just (x,a)) = a >> tryReadChan v {- we have a possible race condition here -} >>= go
```

There are many variants each with it's own tradeoffs so one a free to build a 
way that matchs his task. But the next things should hold:

  1. Datastructure should either guarantee that other side will read resource
  atomically or be registered in ResourceT monad
  2. Read and writes should be exception safe
