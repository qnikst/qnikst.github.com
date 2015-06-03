---
author: Alexander Vershilov
date:   2015-06-03
title: Things to know about exception handling.
---

Let's take a look at the simple pattern, in this pattern we want to write
an endless program for some purpose:

```haskell
runit f = f `onException` (putStrLn "exception!" >> runit f)
```

Do you see a huge problem here? And this problem is not with code
accuracy, inability to gracefully exit or something like that. There is another
problem in this pattern. If you don't then continue reading.

Assume `f` is some simple function that loops (for example `let f = forever yield`). Try to answer the following questions

  1. what will happen if you run `runit f` in a separate thread
     (`forkIO $ runit f`)?

  2. what will happen if you send an exception to the forked thread?

  3. what will happen if you send another exception to that thread?

Now you can try to check your guesses in ghci. If everything is ok
then there is nothing new to read for you. However if you are lazy
(and didn't try to answer or run the code), you can continue.


Let's try to test:

```haskell
> :m Control.Exception Control.Concurrent GHC.Conc Control.Monad
> let runit f = f `onException` (putStrLn "exception!" >> runit f)
> x <- forkIO $ runit $ forever yield
> x
ThreadId 154
> threadStatus x
ThreadRunning
```

Ok everything as expected, thread is running and consuming 100% of one CPU.
Now go to the step 2.:

```haskell
> throwTo x (userError "foo")
exc> eption!
> threadStatus x
ThreadRunning
```

Exception is caught, everything is OK. Now go to the 3.

```haskell
> throwTo x (userError "foo")
```

Hah.. It just hangs!!! We can interrupt it and check thread status

```haskell
> threadStatus x
ThreadRunning
```

This is definitely not what could be expected. Lets try to understand what
had happened.

As you know exception sending is synchronous in a sense, that `throwTo` call
will not exit unless exception will be delivered to the thread (or the thread dies).

Quote from [haddock](https://hackage.haskell.org/package/base-4.7.0.2/docs/Control-Concurrent.html):

> Exception delivery synchronizes between the source and target thread: `throwTo` does not return until the exception has been raised in the target thread. The calling thread can thus be certain that the target thread has received the exception. Exception delivery is also atomic with respect to other exceptions. Atomicity is a useful property to have when dealing with race conditions: e.g. if there are two threads that can kill each other, it is guaranteed that only one of the threads will get to
kill the other.

It's definitely not an issue of non-reaching safepoint, that is also possible. Another quote:

> In GHC, an exception can only be raised when a thread reaches a safe point, where a safe point is where memory allocation occurs. Some loops do not perform any memory allocation inside the loop and therefore cannot be interrupted by a `throwTo`.

It could be a case if `f` would be something like `f = return $ last [1..]` compiled with optimizations
turned on. But as we have seen the first exception was delivered successfully.
And also we are not in a *FFI* call. The only left possibility is that the thread is in a masked state.
Let's check it:

```
> x <- forkIO $ runit (getMaskingState >>= print  >> forever yield)
Unmasked
Prelude Control.Exception Control.Concurrent GHC.Conc Control.Monad> throwTo x (userError "bar")
exceptiPrelude Control.Exception Control.Concurrent GHC.Conc Control.Monad> on!
MaskedInterruptible
```

Yes, everything happened as we were expecting the thread is now in a masked state,
and so exception can't be delivered. But why? Maybe we need to take a look at
`Control.Exception` module documentation? You may try, but at least in base-4.8.0
(and in the previous versions) these details were not documented.

But we can try to find out solution somewhere else, for example in RTS documentation:

> A thread can request that asynchronous exceptions not be delivered
> ("masked") for the duration of an I/O computation. The primitives
>
> maskAsyncExceptions# :: IO a -> IO a
>
> and
>
> maskUninterruptible# :: IO a -> IO a
>
> are used for this purpose. During a masked section, asynchronous
> exceptions may be unmasked again temporarily:
> 
> unmaskAsyncExceptions# :: IO a -> IO a
> 
> Furthermore, asynchronous exceptions are masked automatically during
> the execution of an exception handler. All three of these primitives
> leave a continuation on the stack which reverts to the previous
> state (masked interruptible, masked non-interruptible, or unmasked)
> on exit.

Looks like a reason of our problem. Unfortunatelly it's not in the documentation
that usual user would read, but a nice thing to know.

And original code should look like the following:

```haskell
runit f = mask $ \release -> do
  ev <- try $ release f
  case ev of
    Left _ -> putStrLn "exception" >> release (runit f)
    Right x -> return x
```

You'll need to pass exception type you are interested in or use `catches`.

I hope you enjoyed reading this, despite a terrible English that I have.
