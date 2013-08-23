----
author: Alexander Vershilov
titile: Automata (draft)
data: 2012/01/21
license: cc-nc-sa
----


About a half year ago I had a next task: I should run a list of 
event listeners on a wire and on each handled event that event 
listener can change (i.e. start listening another event, produce new
event listeners, and send requests to the wire). Yes, sound like
a FRP task, and once you are familar to FRP this article is not 
interested for you, except you may try to help me to generalize
all logic. 

I tried to use monadic approach, however I had too much
problems because monads can bind and run opaque functions while
all theese computation had to carry additional information, so I
end up with function approach: each function returns a command and
next function and special runner change it's state based on this 
information but I ended with some problems: functions were bloated,
function should be written from the end to begining, all additional
variables should be passed explicitly through the functions, 
function can't be composed as they had types constraints. It
was a hell.. A day ago I have read the great arrows totorial 
http://ertes.de/new/tutorials/arrows.html by Ertugrul Söylemez, and
realized that it was an arrow, after some thinking I've found a nice
solution. 

This is a literate haskell post so you can just copy it and compile.

We will need it at the very end to show arrow notation
 
> {-# LANGUAGE Arrows #-}

We need this for defining new arrow instance.

> import Prelude hiding ((.), id)
> import Control.Arrow
> import Control.Category

We need this for describing outer world

> import Control.Monad
> import Control.Concurrent.STM
> import Control.Concurrent
> import Control.Exception

Let's prepare to solve our problem. At first we need to define
an outer world bus i.e. an computation that can recieve requests,
and sends responce. We define 2 channels for this purpose:
input - requests, output - responce. This will not break generalization
as you always can insert a channel based proxy between your computation
and real-world bus.

So our realworld wrapper will look like:

> run :: FIO a b c -> Generator b c -> b -> IO a
> run f g i = 
>   bracket (do in_  <- newTChanIO 
>               out_ <- newTChanIO 
>               t <- forkIO $ g in_ out_
>               atomically $ writeTChan in_ i
>               return (t,in_,out_))
>           (\(t, _, _) -> killThread t)
>           (\(_, i, o) -> f i o)

, where FIO and Generator are helper types:

> type FIO a b c = TChan b -> TChan c -> IO a
> type Generator b c = TChan b -> TChan c -> IO ()

A side node that we need to pass initial value as we use bus-driven logic: 
a message from the bus starts computaion.

Let's write some generators:


> idGenerator :: Generator a a
> idGenerator = fGenerator id
> fGenerator :: (a -> b) -> Generator b a
> fGenerator f ic oc = forever . atomically $ readTChan oc >>= writeTChan ic . f

Now we can review our problem. 

We need to define an finite automata, that
can either finish it's computation or return next state. Will 
write a type for it:

1. it will be a newtype (let's name Auto 2) that has step function

   newtype Auto2 <..> = Auto2 { stepAuto :: <..> }

2. as it's an automation it should get input value of type a and return
either a new value b or new computation:
  
  newtype Auto2 <..> a b = Auto2 { stepAuto :: a -> Either <..> b }

3. we need to carry information about real world bus through all 
computations and it can't change
  
  newtype Auto2 i o a b = Auto2 {stepAuto :: a -> Either (o, <..>) b}

4. final note is that if we return a new value request than our
type is bound by RW-bus: Auto i o i b. So finally we get:


> newtype Auto2 i o a b = Auto2 { stepAuto :: a -> Either (o, Auto2 i o i b) b}

[TBD: I need to add a nice pictures rendered in latex]

This is not a function, this is a computation, so we need to write 
explicit runner to make it work:

> runner auto i o = do
>     x <- atomically $ readTChan i
>     putStr $ "input: "++(show x) 
>     let ret = stepAuto auto x
>     case ret of
>       Left (req, next) -> do
>           putStrLn $ " requesting: " ++ show req
>           atomically (writeTChan o req)
>           runner next i o
>       Right ok -> putStrLn (" result: "++show ok) >> return ok

Will write a simple test function that require incremented value:

> upTo :: Int -> (Int -> Int) -> Auto2 Int Int Int Int
> upTo n f = Auto2 $ \x -> if x >= n
>                      then Right $ f x
>                      else Left ((x+1),  upTo n f)

*Main>  run (runner (upTo 2 (*2))) (idGenerator) 0

Loading package array-0.4.0.1 ... linking ... done.

Loading package stm-2.4.2 ... linking ... done.

input: 0 requesting: 1

input: 1 requesting: 2

input: 2 result: 4

4

So far so good but, but I'd like not to write requests explicitly,
rather to use some 'request' function that will break flow:

> request :: o -> Auto2 i o a i
> request req = Auto2 $ \_ -> Left (req, Auto2 $ \y -> Right y)

*Main> run (runner (request 5)) idGenerator 0

input: 0 requesting: 5

input: 5 result: 5

5

One thing is bad: we need an input to request a state, that input
will be ignored, it seems that it's not a problem and will never
hit user, however I have no strong explanation.

Now we need a way to compose such computation, not a problem, 
because this is a Category, so we need just to write an instance:

> instance Category (Auto2 i o) where
>     id = Auto2 $ \x -> Right x
>     auto2 . auto1 = Auto2 $ \x ->
>               let out1 = stepAuto auto1 x
>               in case out1 of
>                   Right b -> stepAuto auto2 b
>                   Left (o,auto1') -> Left (o, (auto2 . auto1'))
> 

id will just return a result and have no effects. Composition will
run internal computation and if it's succeed start outer, otherwise
continue to run new inner automata untill it succeeds.

At this point we doesn't gain many advantages as we have only 
composition of automata, and will have a problems once we will leave
the types pipeline.

Now we'll define an arrow instance so we will be able to lift opaque
functions to the Automata level and create a side channels to carry
values alongside with computation:

> instance Arrow (Auto2 i o) where
>     arr f = Auto2 $ \x -> Right (f x)
>     first (Auto2 f) = Auto2 $ \(x, y) -> 
>         let next = f x
>         in case next of
>             Right b -> Right (b, y)
>             Left  a -> arec (\z -> (z,y)) a
>
> arec g (o, Auto2 f) = Left $ 
>          (o, Auto2 $ \x ->
>               let next = f x
>               in case next of
>                   Right b -> Right (g b)
>                   Left y  -> arec g y)

Arrow instance just lift a pure function to Automata level, and first
run recursive automata and store result in the first box. This is great
we can just compose our computation in a straight forward way saving
a results alongside a computation

> test1 = request 5 >>>
>         arr (\x -> (x,x)) >>>
>         first (request 6) >>>
>         arr (\(x,y) -> x+y)

*Main> run (runner (test1)) (idGenerator 0)

input: 0 requesting: 5

input: 5 requesting: 6

input: 6 result: 11

11

> test2 = arr (\x -> ((),x)) >>>
>         first (request 4)  >>>
>         arr (\(x,y) -> if y>5 then x+y else x-y)

*Main> run (runner (test2)) idGenerator 6

input: 6 requesting: 4

input: 4 result: 10

10

*Main> run (runner (test2)) idGenerator 4

input: 4 requesting: 4

input: 4 result: 0

0

As I said 'request' is not a problem as we can feed it with our internal
value. Maybe we need to use Void there..

But that's not all we want to be able to choise between computations

> instance ArrowChoice (Auto2 i o) where
>   -- left :: a b c -> a (Either b d) (Either c d)
>   left (Auto2 f) = Auto2 $ \x -> case x of
>           Left b -> let next = f b
>                     in case next of
>                         Right c -> Right (Left c)
>                         Left l -> arec Left l
>           Right d -> Right (Right d)
>   

> test3 :: Auto2 Int Int Int Int
> test3 = arr (\y -> if y > 5 then Left y else Right y) >>>
>         left (request 3) >>>
>         right (request 7) >>>
>         arr (\x -> case x of
>                 Left x  -> x
>                 Right y -> y)

*Main> run (runner (test3)) idGenerator 1

input: 1 requesting: 7

input: 7 result: 7

7


*Main> run (runner (test3)) idGenerator 6

input: 6 requesting: 3

input: 3 result: 3

3


The only problem that it's not very easy to write in such a style,
thats what an arrow notation is done for:

> 
> test4 = proc x -> do
>     if x > 5 
>         then request 0 -< () 
>         else request 10 -< ()

*Main> run (runner (test4)) idGenerator 6

input: 6 requesting: 0

input: 0 result: 0

0

*Main> run (runner (test4)) idGenerator 1

input: 1 requesting: 10

input: 10 result: 10

10


Thats all, and thats really awesome: no more explicit function carrying,
explicit RW-bus communication that implicilty coupled with all control
flow.

At the end I should note, that this is not all functionallity that was
in my module, so I'm brifly name what is not present here and how to add it:

  1. There is only one event listener. To fix it one just need to add a state
  to the runner that state should save all listeners, feed them input and
  collect output. It can be done in parallel (explicitly via forkIO/channels
  or implicilty via par)

  2. Listener can't produce new listeners, it's just a problem of outer type,
  it can be changed to o' = (o, [Auto2]), where fst is result and Auto2 is
  new listeners.
  
  3. Listener can send only once command at a time -  can be fixed by changing
  o to [o'].

  4. Listeners handle any event, there are two options: one write a wrapper that
  will validate any input and either return an unchanged automata with null 
  request or all automatas should be tagged by their receive type and stored in
  a map.


One big note this is just a learning matherial, if you want to do real world,
start reading Ertugrul Söylemez and others about FRP.

Help required:

  * if you see any mistake feel free to correct me

  * I don't know how to correctly show a executed code blocks it lhs, if you
  know 

