---
title: Playing with trees: prefix map 
author: Alexander Vershilov
date: January 1, 2013
tags: haskell
license: by-nc-sa
---

We introduce a binary tree like data structure with next structure.


> {-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
>
> import Prelude hiding (head, length, drop, take, lookup, null)
> import Data.Function
> import Data.ByteString.Char8 hiding (empty)
> import Test.QuickCheck
>
> --        [Node (current value) l v r eq]
> --                              | | |  +------------------------------------+
> --        +---------------------+ | +------------------+                    |
> --        |                       |                    |                    |
> --        +                       |                    +                    |
> --  element less            value or nothing        elements that        elements that
> --  than current            if it intermideate      are more then        have <current value>
> --                              node                current              as prefix
> -- 


Top level item represent empty value and can have a value.

> type PrefixMap a = (Maybe a, PMap a)

Inner tree is either an empty value or node, that has left/right children
and maybe can have a value and next element

> data PMap  a = E
>              | N ByteString (PMap a) (Maybe a) (PMap a) (PMap a)
>              {-   current    less      value        more    eq   -}
>              deriving (Show)

Having PrefixMap as a additional layer we can assume, that we have a non-null
prefix on each level.


Introduce simple builders

> empty :: PrefixMap a
> empty = (Nothing, E)
>
> node :: ByteString -> a -> PrefixMap a
> node b a | null b    = (Just a, E) 
>          | otherwise = (Nothing, N b E  (Just a)  E E)

Now inserting elements it's a bit tricky and may be simplified in 
the way of removing not needed insances

> insert :: ByteString -> a -> PrefixMap a -> PrefixMap a
> insert b a (v,n) | null b    = (Just a, n)
>                  | otherwise = (v, inner b a n)

> inner :: ByteString -> a -> PMap a -> PMap a
> inner b a E = N b E (Just a) E E
> inner b a n@(N b' l v r e) | null b     = n
>                            | otherwise  = 
>   case comparing head b b' of
>     LT -> N b' (inner b a l) v r e   -- value less then current
>     GT -> N b' l v (inner b a r) e   -- value more then current
>     EQ -> let x = commonPart b b' -- value has common part
>               c = take x b
>               c'= take x b'
>               n' = N (drop x b') E v E e
>           in if on (==) length c b'       -- b' isPrefix of b
>                  then 
>                   if on (==) length c b    -- b' == b 
>                       then N c l (Just $! a `fq` v) r e
>                       else N c l v r (inner (drop x b) a e) -- [b < b']
>                  else -- [ c < b ]
>                   if on (==) length c b
>                       then N c' l (Just a) r n'
>                       else N c  l Nothing  r (inner (drop x b) a n')
>   where 
>     fq a _ = a
 

lookup function

> lookup :: ByteString -> PrefixMap a -> Maybe a
> lookup b (v, n) | null b = v 
>                 | otherwise = lookinner b n

> lookinner :: ByteString -> PMap a -> Maybe a
> lookinner b E = Nothing
> lookinner b (N b' l v r e) =
>   case comparing head b b' of
>     LT -> lookinner b l
>     GT -> lookinner b r
>     EQ -> let x = commonPart b b'
>           in if x == length b'
>                 then if x == length b then v else lookinner (drop x b) e
>                 else Nothing
         
> commonPart :: ByteString -> ByteString -> Int
> commonPart a b = go 0
>   where 
>     go :: Int -> Int
>     go x | x == y                = x
>          | on (==) (findex x) a b = go (x+1)
>          | otherwise             = x
>     y = on min length a b
>     findex = flip index
>     {-# INLINE findex #-}
> 
> comparing = on compare
 
Check if we are right

> prop_InsertList (ls::[String]) = 
>   let x = Prelude.foldl (\o x -> insert (pack x) (pack x) o) empty ls
>   in Prelude.all (\l -> (l=="") || pack l `lookup` x == Just (pack l)) ls
>
> main = quickCheck prop_InsertList
> 

What interesting is what properties to we have, ideally we can rewrite 
code thinking of a  N c l v r e as a Tree (M v e)

Caveats:

  * this tree is unbalanced so we don't have best case: this can be fixed
    by rewriting structure as RB-tree so tree on each level will be sorted.

  * this tree doesn't pack data as it possible: to pack data correctly one
    need to store a lenght of full bytestring in each node and replace element
    by the longer string, and copy bytestiring at the leaf node. It this
    variant we will smallest overhead.

  * Node can be rewritten as N (PMap a) (PMap a) (PrefixTree a) this will
    add a level of indirection but will simplify an insert and lookup a 
    bit.


