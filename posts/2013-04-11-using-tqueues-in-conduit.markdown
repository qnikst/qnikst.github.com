----
author: Alexander Vershilov
title: Using queues in conduits
date: 2013-04-11
tags: haskell
license: by-nc-sa
----

Sometime when you use conduit you need to split processes for example
when you have CPU bound and IO bound tasks thus you will have better 
workload. Also you may need to get events from different sources concurrently, and 
for such purposes you may want to use [stm-conduit](http://hackage.haskell.org/package/stm-conduit) library. 


This library provides conduits over STM primitives: 

  * [TMChans](http://hackage.haskell.org/packages/archive/stm-chans/1.3.1/doc/html/Control-Concurrent-STM-TMChan.html#t:TMChan)  - closable (finite) STM channels
  * [TBMChans](http://hackage.haskell.org/packages/archive/stm-chans/1.3.1/doc/html/Control-Concurrent-STM-TBMChan.html#t:TBMChan) - closable (finite) bounded STM channels
  * [TQueue](http://hackage.haskell.org/packages/archive/stm/2.4.2/doc/html/Control-Concurrent-STM-TQueue.html#t:TQueue)   - infinite STM Queues
  * [TBQueue](http://hackage.haskell.org/packages/archive/stm/2.4.2/doc/html/Control-Concurrent-STM-TBQueue.html#t:TBQueue)  - infinite bounded STM Queues

and closable queues comming soon.

Here closable means that you can close a primitives outside it, so source will report that it is
closed on the opposite infinite primitive will wait for an input forever, so you should use
another conduit to handle end of a stream or send an asynchronous exception.

Bounded means that there is a limit of pending messages inside primitive and if that limit is
reached any thread that wants to write to the primitive will be blocked on input. This is
basically what you need to guarantee limited (or even constant) memory usage.

New primitive TQueue was added in stm-2.3 and provides shared amortized queue that is
faster than TChan/Chan but doesn't provide dupTChan, cloneTChan primitives.
