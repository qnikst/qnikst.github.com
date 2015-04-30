-----
title: Logging all exceptions in ghci
author: Alexander Vershilov
date: 2015-04-30
tags: haskell, exceptions, tips
license: by
-----

On one resource there was a question how to log all exception in GHCI.
There was a use case where user could start a background processes
and some exceptions could be lost. 

In a normal program you can use  `-prof` compilation option and run
program with `+RTS -xc` in order to get exceptions messages and stack-traces
logged. However I assume that most of the ghc users doesn't have `ghc`
compiled with profiling flags. So we need to find another option.

And there is a solution, as every forked thread (that uses `forkIO`
installs a exceptions handler:

From [GHC.Cons.Sym](https://hackage.haskell.org/package/base-4.8.0.0/docs/src/GHC-Conc-Sync.html#forkIO)
```haskell
forkIO :: IO () -> IO ThreadId
forkIO action = IO $ \ s ->
   case (fork# action_plus s) of (# s1, tid #) -> (# s1, ThreadId tid #)
    where
      action_plus = catchException action childHandler
```

From [GHC.IO]()
```haskell
{-
catchException used to handle the passing around of the state to the
action and the handler.  This turned out to be a bad idea - it meant
that we had to wrap both arguments in thunks so they could be entered
as normal (remember IO returns an unboxed pair...).

Now catch# has type

catch# :: IO a -> (b -> IO a) -> IO a

(well almost; the compiler doesn't know about the IO newtype so we
have to work around that in the definition of catchException below).
-}
catchException :: Exception e => IO a -> (e -> IO a) -> IO a
```

In order to set hander we can use:
```haskell
setUncaughtExceptionHandler :: (SomeException -> IO ()) -> IO ()
```

In order to add something to ghci we can load a script by passing it with 
`-ghci-script` option

```haskell
:m GHC.Conc.Sync Control.Exception

:{
let uncaughtExceptionHandler :: SomeException -> IO ()
    uncaughtExceptionHandler e = do
        putStrLn $ "Unhandled exception: " ++ show e
:}

:{
let setDefaultUncaughtExceptionHandler :: IO ()
    setDefaultUncaughtExceptionHandler =
       setUncaughtExceptionHandler uncaughtExceptionHandler
:}

:m -GHC.Conc.Sync Control.Exception
```

And load it with `ghci -ghci-script script.hs`.

```
qnikst@localhost ~ $ ghci -ghci-script script.hs 
GHCi, version 7.8.4: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :m +Control.Exception Control.Concurrent
Prelude Control.Exception Control.Concurrent> forkIO $ threadDelay 10000 >> error "yo!"
ThreadId 28
Prelude Control.Exception Control.Concurrent> ghc: yo!
```

