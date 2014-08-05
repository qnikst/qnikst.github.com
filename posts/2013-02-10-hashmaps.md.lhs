Recently I've registered on the (exercism.io)[http://exercism.io] site and
solved haskell tasks there. I'm recommending that site for both beginers
and advanced users, as beginers may learn from the community review and 
advanced users may review and comment themselve. This post is not very interesting
as it doesn't contain any deep analisys or interesting notes.

There were to interesting tasks that may be solved with or without hashtables
and I wanted to benchmark how hashtables may impact performance of that tasks.
If you are going to participate in exercism here will be spoilers

The first task is Anagram:

> {-# LANGUAGE TupleSections #-}
> import qualified Data.HashMap.Strict as HM
> import qualified Data.Map.Strict as M
> import Data.Char
> import Data.List ( sort )
> import Criterion.Main

Simple version 

> -- by me
> anagramsFor :: String -> [String] -> [String]
> anagramsFor x = filter ((== xn) . normalize) . filter (/=x)
>   where
>      normalize = sort . map toLower
>      xn = normalize x

HashMap version:

> -- by (@ethercrow)[http://exercism.io/ethercrow]
> anagramsFor1 :: String -> [String] -> [String]
> anagramsFor1 term
>    = filter (\candidate -> candidate /= term
>                && letterCount candidate == termLetterCount)
>  where termLetterCount = letterCount term
>
> letterCount :: String -> HM.HashMap Char Int
> letterCount = HM.fromListWith (+) . fmap ((, 1) . toLower)

And anagrams:

> scoresM :: M.Map Char Int
> scoresM = M.fromList scores
>
> scoresH :: HM.HashMap Char Int
> scoresH = HM.fromList scores
> 
> scores :: [(Char,Int)]
> scores =  [ (l,c) |
>     (ls,c) <- [ ("AEIOULNRST",1)
>               , ("DG",2)
>               , ("BCMP",3)
>               , ("FHVWY",4)
>               , ("K",5)
>               , ("JX",8)
>               , ("QZ",10)],
>     l <- ls
>   ]
> 
> 
> scoreWord :: String -> Int
> scoreWord = sum . map scoreLetter
>   where
>     scoreLetter :: Char -> Int
>     scoreLetter = flip (M.findWithDefault 0) scoresM . toUpper
>
> 
> scoreWord1 :: String -> Int
> scoreWord1 = sum . map scoreLetter
>    where
>       scoreLetter :: Char -> Int
>       scoreLetter = flip (findWithDefault 0) scoresH . toUpper
>       findWithDefault v k m = case k `HM.lookup` m of
>         Nothing -> v
>         Just x  -> x
 

> main = defaultMain
>  [ bgroup "anagram"
>     [ bench "simple"  $ nf (anagramsFor "Orchestra") ["cashregister", "Carthorse", "radishes"]
>     , bench "hashmap" $ nf (anagramsFor1 "Orchestra") ["cashregister", "Carthorse", "radishes"]
>     ]
>  , bgroup "scrabble" 
>     [ bench "simple"  $ nf scoreWord "cashregister"
>     , bench "hashmap" $ nf scoreWord1 "cashregister"
>     ]
>  ]

Results:

```
warming up
estimating clock resolution...
mean is 1.892643 us (320001 iterations)
found 58769 outliers among 319999 samples (18.4%)
  54086 (16.9%) low severe
  4683 (1.5%) high severe
estimating cost of a clock call...
mean is 50.56212 ns (12 iterations)
found 1 outliers among 12 samples (8.3%)
  1 (8.3%) high mild

benchmarking anagram/simple
mean: 5.985313 us, lb 5.973008 us, ub 6.000866 us, ci 0.950
std dev: 70.55760 ns, lb 57.56464 ns, ub 89.65218 ns, ci 0.950

benchmarking anagram/hashmap
mean: 8.926868 us, lb 8.689560 us, ub 9.300685 us, ci 0.950
std dev: 1.494973 us, lb 1.058908 us, ub 1.992894 us, ci 0.950
found 15 outliers among 100 samples (15.0%)
  3 (3.0%) high mild
  12 (12.0%) high severe
variance introduced by outliers: 91.528%
variance is severely inflated by outliers

benchmarking scrabble/simple
mean: 1.323841 us, lb 1.321476 us, ub 1.326522 us, ci 0.950
std dev: 12.91355 ns, lb 11.27313 ns, ub 15.02281 ns, ci 0.950

benchmarking scrabble/hashmap
mean: 1.276473 us, lb 1.254422 us, ub 1.320440 us, ci 0.950
std dev: 151.2879 ns, lb 81.85053 ns, ub 237.6315 ns, ci 0.950
found 11 outliers among 100 samples (11.0%)
  11 (11.0%) high severe
variance introduced by outliers: 84.196%
variance is severely inflated by outliers
```

Interesing enough that ghc-7.8 results are code slighly worse.

