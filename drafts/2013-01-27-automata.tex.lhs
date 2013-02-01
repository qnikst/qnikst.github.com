----
title: Dynamic event handling model using Arrows
author: Alexander Vershilov
data: 2012/01/21
----

\begin{abstract}
This post describes a simple approach to dynamic \small{(i.e. a new
event handlers can appear in runtime)} event handling 
that gives a way to write complex event handlers with feedback 
in a natural way. This approach is interesting as a first step 
to create a complex FRP system, and for simple tasks where 
such solution is a sufficient.
\end{abstract}

\section*{Preface}

About a half year ago I had a next task: I should run a list of 
event listeners on a wire and on each handled event event 
listeners can change (i.e. start listening another event, produce new
event listeners, and send requests to the wire). It sounds like
a FRP task, and once you are familar to FRP this post may be not 
interesting for you, except you may try to help me to generalize
all logic.

I tried to use monadic approach, however I had too much
problems because monads can bind and run opaque functions while
all these computation had to carry additional information, so I
end up with function approach: each function returns a command and
next function and special runner change it's state based on this 
information but I ended with some problems: functions were bloated,
function should be written from the end to begining, all additional
variables should be passed explicitly through the functions, 
function can't be composed as they had types constraints. It
was a hell.. Few days ago I have read the great a
\href{http://ertes.de/new/tutorials/arrows.html}{arrows tutorial} by 
Ertugrul Söylemez, and realized that it was an arrow, after some 
thinking I've found a nice solution. 

\section{Problem}

This post is a literate haskell post so you can copy in and run in 
ghci. So at first we will add some imports:

We need this for defining new arrow instance.
\begin{code}
{-# LANGUAGE Arrows #-}
import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
\end{code}

We need theese imports to define External world.

\begin{code}
import Control.Applicative
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception
import Data.Monoid
\end{code}

Assume have an External interface, this means some asynchonous interface to 
external system that have a simple API:

\begin{code}
data External a b = External 
        { input :: a -> IO () -- ^ write to a wire
        , output :: IO b      -- ^ read a wire
        }
\end{code}

This interface has next properties: 

\begin{itemize}
  \item we can write requests and interface will asynchonously answer
  \item interface can generate events on it's own.
\end{itemize}

We want to be able to write an event handlers that can catch events
that are needed, request a wire and get responces. So we want to be able to

\begin{enumerate}
  \item catch events that we need
  \item request wire
  \item carry a local variables
  \item have if-branches
  \item have a possibility to write all above in a natural way
\end{enumerate}

What it meast that we want to be able to write a handlers in the next way:

\begin{verbatim}
desiredHandler = do
  e <- catchEvent -- 1
  let val = ...   -- 3
  resp <- request req -- 2
  if f resp 
    then monadic1ThatUsesVal
    else monadic2ThatUsesVal
\end{verbatim}

In the main part of this article we will ignore 1 and it will be shown
that we don't loose generality. So now we think that we match any event.
Also we will use only one event handler, instead on a multiple ones, like
above we doesn't loose any generality and will show a way to improve this.

Without 4 and 5, we can just use have a function (F:: (c -> Either (b,F) d), that
returns either a next step or a result. There are a several problems in this
aproach, all сводятся к that this functions doesn't form a monad. This is 
because monad is a sequencial computation that can lift opaque function into it,
while we should have a computation that ca. The solution is to write a category 
and an arrow notation.

In second section we will set up our world will write our datatype, in 
section 3 we will write a request reply functionality over the wire and
set up instances we need. In section 4 we will take a look at events and
write helpers. Section 5 describes how to run multiple handlers in parrallel
and some words about parallelization.

\section{External world}

At first we need to write an External instance that we will use in our 
program. We will use 2 STM channels one for requests another for responces
and events:

\begin{code}
initialize :: IO (External a b, (TChan a, TChan b))
initialize = do
      i <- newTChanIO
      o <- newTChanIO
      return (External { input = atomically . (writeTChan i)
                       , output = atomically . readTChan $ o}, (i, o))
\end{code}

At this moment we again doesn't loose any generality as we always can
write such a wrapper for any type of IO communication. (TODO examples?).

To emulate an exernal system responces we will use a generator, this a function
that generates a request for the given responce:

\begin{code}
type Generator a b = TChan a -> TChan b -> IO ()

idGenerator :: Generator a a
idGenerator = fGenerator id
fGenerator :: (a -> b) -> Generator a b
fGenerator f ic oc = forever . atomically $ readTChan ic >>= writeTChan oc . f
\end{code}

Now we can provide an environment function for our experiments,
note that because of our logic is bus driven we need to pass 
first value to initialize logic.

\begin{code}
experiment' f g i (e, c@(in_,out_)) = 
  bracket (forkIO $ atomically (writeTChan in_ i) >> uncurry g c)
          killThread
          (const $ f e)
experiment f g i = initialize >>= experiment' f g i
\end{code}

\subsection{Request-Reply}

Now we can construct our datatypes. 

We need to define an finite automata, that can either finish it's computation 
or return next state. Will write a type for it:

\begin{enumerate}
  \item it will be a newtype (let's name Auto 2) that has step function

   newtype Auto2 <..> = Auto2 { stepAuto :: <..> }

  \item as it's an automation it should get input value of type 'a' and return
either a new value 'b' or new computation:
  
  newtype Auto2 <..> a b = Auto2 { stepAuto :: a -> Either <..> b }

  \item we need to carry information about external bus through all 
computations, so we can request it on any level and theese types 
can't change:
  
  newtype Auto2 i o a b = Auto2 {stepAuto :: a -> Either (o, <..>) b}

  \item final note is that if we return a new value request than our
type is bound by external-bus: Auto i o i b. So finally we get:
\end{enumerate}

\begin{code}
newtype Auto2 i o a b = Auto2 { stepAuto :: a -> Either (o, Auto2 i o i b) b}
\end{code}

As our datatype is a computation, and not a function we need to write 
explicit runner: function that will call our computation.

\begin{code}
runner :: (Show i, Show o, Show b) => External o i -> Auto2 i o i b -> IO b
runner ext auto = do
    x <- output ext
    putStr $ "received: " ++ show x
    let ret = stepAuto auto x
    case ret of
      Left (req, next) -> do
          putStrLn $ " requesting: " ++ show req
          input ext req
          runner ext next 
      Right ok -> putStrLn (" result: " ++ show ok) >> return ok
run f g = runner g f
\end{code}

This is a very basic function that receives new signal from wire, 
and feeds in into our computation and then either continues or
finishes.

Lest demonstate how it will work:

\begin{code}
upTo :: Int -> (Int -> Int) -> Auto2 Int Int Int Int
upTo n f = Auto2 $ \x -> if x >= n
                     then Right $ f x
                     else Left ((x+1),  upTo n f)
\end{code}

This function will request a new value while it's less than
first param. Here is an output:

\begin{verbatim}
*Main> experiment (run (upTo 2 (*2))) (idGenerator) 0
received: 0 requesting: 1
received: 1 requesting: 2
received: 2 result: 4
4
\end{verbatim}

So far so good but, but I'd like not to write requests explicitly,
rather to use some 'request' function that will break flow:

\begin{code}
request :: o -> Auto2 i o a i
request req = Auto2 $ \_ -> Left (req, Auto2 $ \y -> Right y)
\end{code}

\begin{verbatim}
*Main> run (run (request 5)) idGenerator 0
input: 0 requesting: 5
input: 5 result: 5
5
\end{verbatim}

One thing is bad: we need an input to request a state, that input
will be ignored, it seems that it's not a problem and will never
hit user, however I have no strong explanation.

Now we need a way to compose such computations, thats not a problem,
because theese computations form a Category, so we need just to 
write an instance:

\begin{code}
instance Category (Auto2 i o) where
   id = Auto2 $ \x -> Right x
   auto2 . auto1 = Auto2 $ \x ->
             let out1 = stepAuto auto1 x
             in case out1 of
                 Right b -> stepAuto auto2 b
                 Left (o,auto1') -> Left (o, (auto2 . auto1'))
\end{code}


\texttt{id} will just return a result and have no side effects. Composition 
\texttt{(.)} will run internal computation and if it's succeed start outer, 
otherwise continue to run new inner automation until it succeeds. Sidenote
there was a different composition behavior Ertugrul`s article, composition 
there nests one arrow inside another.

At this point we doesn't gain many advantages as we have only 
composition of automata, and will have a problems once we will leave
the types pipeline.

Now we'll define an arrow instance so we will be able to lift opaque
functions on the Automation level and create a side channels to carry
values alongside with computation (instead of let bindings in monad 
form that are visible downside the binding):

\begin{code}
instance Arrow (Auto2 i o) where
    arr f = Auto2 (\x -> Right (f x))
    first (Auto2 f) = Auto2 (\(x, y) -> zrec (\z -> (z,y)) (f x))
\end{code}

\texttt{arr} just lift a pure function on the Automation level, and 
\texttt{first} run recursive automation and store result in the first channel, 
leaving second unchanged. Now we have a straightforward 
way of saving a results alongside a computation.

We define a helper function that will recurce over automation and
apply a function to the final result:

\begin{code}
zrec g (Right x) = Right $ g x
zrec g (Left (o, Auto2 f)) = Left (o, Auto2 $ \x -> zrec g (f x))
\end{code}

A small demonstration:

\begin{code}
test1 = request 5 >>>
        arr (\x -> (x,x)) >>>
        first (request 6) >>>
        arr (\(x,y) -> x+y)
\end{code}

\begin{verbatim}
*Main> experiment (runr (test1)) (idGenerator 0)
input: 0 requesting: 5
input: 5 requesting: 6
input: 6 result: 11
11
\end{verbatim}

\begin{code}
test2 = arr (\x -> ((),x)) >>>
        first (request 4)  >>>
        arr (\(x,y) -> if y>5 then x+y else x-y)
\end{code}

\begin{verbatim}
*Main> experiment (run test2) idGenerator 6
input: 6 requesting: 4
input: 4 result: 10
10

*Main> experiment (run test2) idGenerator 4
input: 4 requesting: 4
input: 4 result: 0
0
\end{verbatim}


As was said before \texttt{request} is not a problem as we can feed it with 
our internal value.

At this moment we are able to carry intermideate values and now
we need to define a way to use if-condution. Using an arrow we have
only a channels abstraction, so we need to use a "conditional"
value in channels and Either is a good candidate for it. Will not
reinvent a wheel and use an olready created an instance:

\begin{code}
instance ArrowChoice (Auto2 i o) where
  -- left :: a b c -> a (Either b d) (Either c d)
  left (Auto2 f) = Auto2 $ \x -> 
    case x of
      Left b  -> zrec Left (f b)
      Right d -> Right (Right d)
\end{code}

Here \texttt{left} takes \texttt{Either a b} value, and if it`s \texttt{Left}
then running a recursive computation and store result in \texttt{Left}.
Otherwise \texttt{Right d} is returned.

Now we have a way to split channel into left and right parts.

An small demonstration:

\begin{code}
test3 :: Auto2 Int Int Int Int
test3 = arr (\y -> if y > 5 then Left y else Right y) >>>
        left (request 3) >>>
        right (request 7) >>>
        arr (\x -> case x of
                Left x  -> x
                Right y -> y)
\end{code}

\begin{verbatim}
*Main> experiment (run test3) idGenerator 1
input: 1 requesting: 7
input: 7 result: 7
7

*Main> experiment (run test3) idGenerator 6
input: 6 requesting: 3
input: 3 result: 3
3
\end{verbatim}

The only problem that it's not very easy to write in such a style,
thats what an arrow notation is done for:

\begin{code}
test4 = proc x -> do
    if x > 5 
        then request 0 -< () 
        else request 10 -< ()
\end{code}

\begin{verbatim}
*Main> run (runner (test4)) idGenerator 6
input: 6 requesting: 0
input: 0 result: 0
0

*Main> run (runner (test4)) idGenerator 1
input: 1 requesting: 10
input: 10 result: 10
10
\end{verbatim}


Thats all, and thats really awesome: no more explicit function carrying,
explicit RW-bus communication that implicilty coupled with all control
flow.


\section{Event handling}

Now lets generalize our approach. First we need to be able to catch only
events we are interested in. We can have a list approaches:

\begin{enumerate}
  \item additional data method
  \item api extension
\end{enumerate}

\subsection{Additional method}

We can rewrite our automation type to

\begin{code}
data Auto3 i o a b = Auto3 {
    checkAuto :: i -> Bool
    , runAuto :: i -> Either (o, Auto3 i o i b) b
    }
\end{code}

In this approach we doesn't need to run computation to check if it input 
matches predicate. But it will lead to a level of rewriting, so we will
not do it unless it's really needed

\subsubsection{Event API}

To describe a list of event we will write a list generator:

\begin{code}
listGenerator :: [b] -> Generator a b
listGenerator ls ic oc = mapM_ (atomically . (writeTChan oc)) ls
\end{code}

The idea for this approach is to add a predicate that will try to convert
an input to an input we need, possibly validating it. (Really we can just
use a predicate and then convert a value to another one)

\begin{code}
type ConvPred i j = (i -> Maybe j)
idConv :: (a -> Bool) -> ConvPred a a
idConv p = \i -> if (p i) then Just i else Nothing
\end{code}

In order to use an API exension we should restrict our output datatype to
the type that supports 0 (\texttt{zero}) a value that means nothing in this
type. We need it because if value doesn't math predicate we should perform
a noop and wait for next value, keeping automation unchanged, so we will
introduce type-class:

\begin{code}
class Zero a where zero :: a
instance Zero (Maybe a) where zero = Nothing
instance Zero [a] where zero = []
\end{code}

We are not using \texttt{Monoid} because we does not require \texttt{mappend}
operation to exist.

Now we can define an event listening arrow, the only problem that we should
feed our arrow with a value to make it run:

\begin{code}
event :: (Zero o) => Auto2 i o i i -> Auto2 i o i i
event a = Auto2 $ \_ -> Left (zero, a)
\end{code}

First version of matcher:

\begin{code}
matchE :: (Zero o) => (ConvPred i a) -> Auto2 i o a b -> Auto2 i o i b
matchE p a@(Auto2 f) = Auto2 $ \x -> 
  case p x of 
    Nothing -> Left (zero, matchE p a)
    Just y  -> zrec id (f y)
\end{code}

Correct version of matcher

\begin{code}
match :: (Zero o) => (ConvPred i b) -> Auto2 i o i b
match p = Auto2 $ maybe (Left (zero, match p)) Right . p
\end{code}

\begin{code}
test6 = experiment (run $ match (idConv (>5)) >>> id)
                   (listGenerator [1..10]) (Just 4)
\end{code}

\begin{verbatim}
*Main> test6
received: 1 requesting: Nothing
received: 2 requesting: Nothing
received: 3 requesting: Nothing
received: 4 requesting: Nothing
received: 5 requesting: Nothing
received: 6 result: 6
6
\end{verbatim}

Now we will write helpers. The easiest one is matchAny:

\begin{code}
matchAny :: (Zero o) => [ConvPred i a] -> Auto2 i o a b -> Auto2 i o i b 
matchAny ps a = matchE (\i -> foldl (\o p -> o <|> p i) Nothing ps) a
\end{code}

Review the algebra of Predicates:

We can intoduce a binary operation 'OR' that splits channels into 2 parts:

\begin{code}
pOr :: ConvPred i a -> ConvPred i b -> ConvPred i (Either a b)
pOr p1 p2 = \i -> Left <$> p1 i <|> Right <$> p2 i 
\end{code}

pOr is composable:

\begin{verbatim}
*Main> :t (idConv (<3)) `pOr` (idConv (>5)) `pOr` (idConv (==42))
  <..> :: ConvPred Int (Either (Either Int Int) Int)
\end{verbatim}

But it's impossible to write a pAnd function as we should somehow carry all
catched variables, so thats what and arrow for.

\begin{code}
pAnd :: ConvPred i a -> ConvPred i b -> ConvPred i (a,b)
pAnd = undefined -- impossible
\end{code}

We can write an automation instance for matchOr:

\begin{code}
matchOr :: (Zero o) => ConvPred i a -> ConvPred i b -> Auto2 i o i (Either a b)
matchOr p1 p2 = match (p1 `pOr` p2)
\end{code}

\begin{verbatim}
test7 = experiment (run $ matchOr (idConv (>5)) (idConv (>3)) >>> id)
                   (listGenerator [6..10]) (Just 4)
\end{verbatim}                   

Now we can write a matchAnd function:

\begin{code}
{-
merge :: Either a a -> a
merge (Left a) = a
merge (Right a) = a

--matchAnd :: (Zero o) => ConvPred i a -> ConvPred i b -> Auto2 i o i (a,b)
matchAnd p1 p2 = 
    matchOr p1 p2 >>> 
    left (arr (\a -> (a,())) >>> second (event $ match p2 >>> id)) >>>
    right (arr (\b -> ((),b)) >>> first (event $ match p1 >>> id)) >>>
    arr merge
-}

both ::(Monoid o) => Auto2 i o i a -> Auto2 i o i b -> Auto2 i o i (a,b)
both (Auto2 f1) (Auto2 f2) = Auto2 $ \x -> 
  case (f1 x, f2 x) of
      (Right a, Right b) -> Right (a, b)
      (Left (v1, a1'), Left (v2,a2')) -> Left (v1 `mappend` v2, both a1' a2')
      (Right a, Left  (v, a2')) -> Left (v, arr (\x -> (a,x)) >>> second a2')
      (Left (v, a1'), Right b) -> Left (v, arr (\x -> (x,b)) >>> first a1')
\end{code}

\begin{code}
test8 = experiment (run $ both (match (idConv odd)) (match $ idConv (>3)))
                   (listGenerator [1..10]) ([4])
\end{code}

Now both is composable:

\begin{code}
test9 = experiment (run $ (match $ idConv odd) `both`
                          (match $ idConv (>3)) `both`
                          (match $ idConv even))
                   (listGenerator [1..10]) ([4])
\end{code}

A few highlevel examples that will wrap all internal automation

\begin{code}
filterI ::(Zero o) => (i -> Bool) -> Auto2 i o i b -> Auto2 i o i b
filterI p a@(Auto2 f) = Auto2 $ \i -> 
    if p i 
      then case f i of
            Left (v, a') -> Left (v, filterI p a')
            Right b -> Right b
      else Left (zero,filterI p a)
\end{code}        

\begin{code}
test10 = experiment (run $ filterI (>5) $ both (match (idConv odd)) (match $ idConv (>3)))
                   (listGenerator [1..10]) ([4])
\end{code}                   

\begin{code}
mapI :: (i -> a) -> Auto2 a o a b -> Auto2 i o i b
mapI g (Auto2 f) = Auto2 $ \i -> 
      case f (g i) of
        Left (v, a') -> Left (v, mapI g a')
        Right b -> Right b
\end{code}        


\section{Multiple event listeners}

Now lets look at the last part of problem: we need to carry a list of handlers,
and create new ones at runtime. We can address this problem in a number of ways.

At first we can write  use a number of runners, each one works with one event handler,
every runner reads from a broadcast TChan, and writes to common channel. This
variant will require no code change, however it requires to run each runner in
a separate thread.

Another variant that we will look at is upgrading our runner to support multiple
runners. Now we need another output datatype, that will carry API for runner, and
now runners returns (). One may want to add additional API functions like delete
listener, or give listener a name.

\begin{code}
data ROutput i o a = Output a 
                   | NewListener (Auto4 i o i ()) (ROutput i o a)
instance (Show a) => Show (ROutput i o a) where
  show (Output a) = show a
  show (NewListener _ i) = "<listener:" ++ show i ++ ">"
\end{code}

Our datatype will look like \texttt{Auto2} except it will use \texttt{ROutput} to
carry information
\begin{code}
newtype Auto4 i o a b = Auto4 { stepAuto4 :: a -> Either (ROutput i o o, Auto4 i o i b) (ROutput i o b)}

instance (Show b) => Show (Auto4 i o a b) where
  show x = "<auto>"
\end{code}

Category instance is not the same as previous as it should carry information about
listeners that should be added from the internal computation

\begin{code}
buildUpdate :: ROutput i o a -> (a, ROutput i o b -> ROutput i o b)
buildUpdate (Output a) = (a, id)
buildUpdate (NewListener a r) = let (x,f) = buildUpdate r in (x,NewListener a . f)

zrec2 g (Right x) = Right $ g x
zrec2 g (Left (o, Auto4 f)) = Left (o, Auto4 $ \x -> zrec2 g (f x))

instance Category (Auto4 i o) where
   id = Auto4 $ \x -> Right (Output x)
   a2@(Auto4 f2) . (Auto4 f1) = Auto4 $ \x ->
             let out1 = f1 x
             in case out1 of
                 Right b -> let (y, g) = buildUpdate b
                            in zrec2 g (f2 y)
                 Left (o, auto1') -> Left (o, (a2 . auto1'))
\end{code}

\begin{code}
addListener a = Auto4 $ \x -> Right $ NewListener a (Output x)
\end{code}

\begin{code}
instance Arrow (Auto4 i o) where
    arr f = Auto4 (\x -> Right (Output $ f x))
    first (Auto4 f) = Auto4 $ \(x, y) -> 
                        zrec2 (\z -> let (z',g) = buildUpdate z in g (Output (z',y))) (f x)
\end{code}

\begin{code}
{- TBD
instance ArrowChoice (Auto4 i o) where
  -- left :: a b c -> a (Either b d) (Either c d)
  left (Auto4 f) = Auto4 $ \x -> 
    case x of
      Left b  -> zrec2 (\z -> let (z',g) = buildUpdate z in Left $ g z' ) (f b)
      --(\z -> let (z',g) = buildUpdate z in g (Output $ Left z)) (f b)
      Right d -> Right $ Output (Right d)
-}
\end{code}

\begin{code}
extractListeners :: ROutput i o a -> ([Auto4 i o i ()], a)
extractListeners = go []
  where go acc (Output x) = (acc, x)
        go acc (NewListener l x) = go (l:acc) x
  

runner2 :: (Show i, Monoid o) => External o i -> [Auto4 i o i ()] -> IO ()
runner2 ext autos = do
    x <- output ext
    putStr $ "received: " ++ show x
    {--- we will take a took at this line --}
    let rets = map (flip stepAuto4 x) autos -- we run all autos 
        (l, r)= unzip $ map results rets
    mapM_ (input ext) r
    runner2 ext (concat l)
  where
--    results :: Either (ROutput i o o, Auto4 i o i ()) (ROutput i o ()) -> ([Auto4 i o i ()],o)
    results (Left (req,next)) = let (ls, x) = extractListeners req
                                in  (next:ls,x)
    results (Right req)       = let (ls, _) = extractListeners req
                                in (ls, mempty)
run2 f g = runner g f
\end{code}


Now we use multiple handlers, the only problem is that we can't start next 
step untils previous is done, and all handlers are run in sequence.

However we can use parallel execution of handlers Either explicitly by
\texttt{forkIO} / \texttt{async}, or by working if we will send requests
previously created workers via STM channel, or implicit parallelistion
by \texttt{parMap rseq}.

If one handler can run very long time then you can hide in behind wrapper.
Here is an idea (however it's not work yet):


\begin{enumerate}
  \item create a wrapper that should return \texttt{TChan} and \texttt{TMVar}
    for responce and fork automation runner
  \item on each query try to get result from \texttt{TMVar} if it's there -
    return take it and proceed as usual, otherwise put request into channel
  \item in automation runner - run automation as usual but when new value is
    requested try to take next value from channel, if it's there - process
    it otherwise but automation into result box.
\end{enumerate}    
