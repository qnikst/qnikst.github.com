Recently on [#ruHakell](https://gitter.im/ruHaskell/forall) gitter channel
there was a question about how to make compiler happy. I managed to fix the
problem, though it was not straightforward. Here I describe a problem and solution.

As the original code is not mine but [@int-index](https://github.com/int-index)
I may be not correct in description on an intent. Also I'll not get deep into the
details of the new extensions, because I may be not correct in all details.

The original code was [https://gist.github.com/int-index/c6b853351912d177cfcefd54678b27ff]().
Copy here for the history:

```haskell
{-# LANGUAGE TypeInType, GADTs, TypeFamilies #-}

import Data.Kind (Type)

data N = Z | S N

data Fin :: N -> Type where
  FZ :: Fin (S n)
  FS :: Fin n -> Fin (S n)

type family FieldCount (t :: Type) :: N

type family FieldType (t :: Type) (i :: Fin (FieldCount t)) :: Type

data T

type instance FieldCount T = S (S (S Z))

type instance FieldType T FZ = Int
type instance FieldType T (FS FZ) = Bool
type instance FieldType T (FS (FS FZ)) = String
```

And type instances didn't typecheck with a very strange error:

```
fieldtype.hs:19:27: error:
    • Expected kind ‘Fin (FieldCount T)’,
        but ‘'FZ’ has kind ‘Fin ('S n0)’
    • In the second argument of ‘FieldType’, namely ‘FZ’
      In the type instance declaration for ‘FieldType’
```

I'm not able to explain why we have that error, and it looks like a bug to me,
or that some [axioms](https://ghc.haskell.org/trac/ghc/wiki/NewAxioms) prevents typechecker
from inferring right types.

So my first solution was to break the task into simpler ones.
First, is that it's easy to write type function that returns field that is needed:

```haskell
type family FiledTypeI (t :: Type) (i :: N) :: Type
type instance FieldTypeI T Z = Int
type instance FieldTypeI T (S Z) = Bool
type instance FieldTypeI T (S (S Z)) = String
````

this works fine, we can check in ghci:

```
*Main> :kind! FieldTypeI T (S Z)
FieldTypeI T (S Z) :: *
= Bool
```

Now we need to be able to convert `Fin N` to the raw `N`, this can be easily done
with a closed type family:

```haskell
type family F2II (a :: Fin n) :: N where
  F2II (FS n) = S (F2 II n)
  F2II FZ     = Z
```

so far so good. Now we can check in the repl:

```haskell
*Main> :kind! FieldTypeI T (F2II (FS FZ :: Fin (FieldCount T)))
FieldTypeI T (F2II (FS FZ :: Fin (FieldCount T))) :: *
= Bool
```

At this point we may expect that everything is ok, and now we can just write a type synonym
to hide ugliness:

```
type DoesNotWork (t::Type) (i :: Fin (FieldCount t)) = FieldTypeI t (F2II i)
```

Here we have all types known and all requred dependencies. But it doesn't work:


```
*Main> :kind! DoesNotWork T (FS FZ)
DoesNotWork T (FS FZ) :: Type
= FieldTypeI T (F2II ('FS 'FZ))
```

even written as a typefamily

```
type family DoesNotWork (t :: Type) (i :: Fin (FieldCount t) where
  DoesNotWork t i = FieldTypeI t (F2II i)
````

Later invesigation showed that `t` and `FieldCount t` on the same side doesn't work well.

So solution here is no remove them. One way is to introduce a `cast` function:

```haskell
type family Cast (t :: N) (i :: Fin n) :: Fin t where
  Cast (S Z) FZ = FZ
  Cast (S n) FZ = FZ
  Cast (S n) (FS k) = FS (Cast n k)
```

Now we can use this cast function to write a nice wrapper:

```
type family FieldType (t :: Type) (i :: Fin p) where
  FieldType t i = FieldTypeI t (F2II (Cast (FieldCount t) i))
```

And everything works!

```
*Main> :kind! FieldType T (FS FZ)
FieldType T (FS FZ) :: *
= Bool
```

However, there are some problems with this solution as we have lost dependency between types
`t` and `Fin (f t)`. This means that if we write unsupported index, we will not be warn
with a nice message from the start, but we will fail to compute `FieldType` function, with
possibly more scary error message.

