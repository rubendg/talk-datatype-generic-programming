{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators, DefaultSignatures #-}
{-# LANGUAGE KindSignatures, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Main where

import GHC.Generics hiding ((:*:), (:+:), Rep1, Rec1, Par1)
import Generics.Deriving.Foldable

-- everything is an expression

x :: Int
x = 2 

-- type annotations are often not necessary due to type inference, 
-- convention is to add explicit types to top-level definitions

y = x + x

-- > :t y

-- values are immutable, we cannot re-assign x

-- x = 3 -- err

-- cannot add two different things

-- z = y + True -- err

-- function definition

f x y = x + y + 2

-- is syntax sugar for

f' = \x -> \y -> x + y + 2

-- function application, associates to the left: ((f 2) 2)

r = f 2 2

{- 

Parameteric polymorphism
- makes the implementation independent of specific instantiations of a type
- and thereby also constraints the number of possible implementations
  
  abstraction/parametricity theorem https://en.wikipedia.org/wiki/Parametricity

-}

duplicate :: a -> [a]
duplicate x = [x, x]

-- leaving functions undefined

add :: Int -> Int -> Int
add = undefined

-- add x y = x + y

-- currying, function type (->) associates to the right

-- add :: Int -> (Int -> Int)

add5 :: Int -> Int
add5 = add 5

-- function composition

add1str x = show (x + 1)

-- no need for x (point free style), abstract out function composition

g `compose` f = \x -> g (f x)

-- :t compose
-- compose :: (b -> c) -> (a -> b) -> a -> c

-- new version

add1str'  = show `compose` (+1)

add1str'' = show . (+1)

-- behaviour, now data

data Alphabet = A | B | C
    deriving Show

-- define functions using pattern matching

encode :: Alphabet -> Int
encode A = 1
encode B = 2

-- note that the match is non-exhaustive, when compiled 
-- this function produces a warning

{-

Ad-hoc polymorphism with Type Classes, compile-time function dispatch on types

Ad-hoc because functions do not have to be defined generically as opposed to
parametric polymorphism.

-}

data Bit = Zero | One deriving Show

class BinaryEncode a where
  biEncode :: a -> [Bit]

instance BinaryEncode Bool where
  biEncode False = [Zero]
  biEncode True  = [One]

-- > biEncode False

-- Polymorphic data types: let's define a polymorphic list

data List a = Nil | List a (List a)
    deriving (Show, Generic1) -- Functor)

-- List is a type constructor and takes a type parameter

-- A kind is the type that describes types
-- > :kind List

-- Now suppose we want to apply some function over the list while preserving 
-- its structure. Notice how the mapping implementation follows the structure 
-- of the data type:

mapList :: (a -> b) -> List a -> List b
mapList f Nil         = Nil
mapList f (List x xs) = List (f x) (mapList f xs)

-- `mapList` is a very specific instance of a more general pattern 
-- that can be best captured by a type class:

-- class Functor (f :: * -> *) where
--   fmap :: (a -> b) -> f a -> f b

-- Let's redefine `mapList`

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f Nil         = Nil
  fmap f (List x xs) = List (f x) (fmap f xs)

exampleList = List 1 (List 2 Nil)

-- > fmap show exampleList

-- Let's define another container type

data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving (Show, Generic1)

exampleTree = Node (Leaf 1) 2 (Leaf 3)

-- the same pattern can be instantiated for different container types:

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- > fmap show exampleTree

doSomething :: (Show a, Functor f) => f a -> f String
doSomething bar = fmap show bar

-- fmap f . fmap g == fmap (f . g) 

{- 
Notice the similarity in the implementations.

Wouldn't it be nice if we could define `fmap` 
for all container types not merely for specific instances?

From an authoritive source (https://www.andres-loeh.de/DGP-Intro.pdf):

"The idea of datatype-generic programming:
  
If we can represent a type as an isomorphic type that is
composed out of a limited number of type constructors, then we
can define a function on each of the type constructors and gain
a function that works on the original type – and in fact on any
representable type."

Hmm what is an isomorphic type? 

Greek/latin Babelfish: iso -> same/equal, morph -> shape/form

A similarly shaped type?

So if we somehow figure out a way to represent our specific type
as a similarly shaped but more abstract type, then we can instead 
try to define our map function on that type, and provided that this can be
done we can reuse the implementation for any representable type.

As such we should be able to define `fmap` in terms of its more generic 
cousin `gmap`:

fmap f = to . gmap f . from

such that we never have to implement type specific fmaps again.
-}

-- How can we encode a choice between two constructors?

type Sum a b = Either a b

-- Multiple choices?

multipleChoice :: Sum Int (Sum Bool String)
multipleChoice = undefined

-- What about combining fields?

type Product a b = (a, b)

-- Multiple fields?

multipleFields :: Product Int (Product Bool String)
multipleFields = undefined

-- What about constructors without fields, like Nil?

type Empty = ()

-- We now have the basic ingredients for representing data types 
-- using a binary sum of products encoding

-- Let's represent Bool

type RepBool = Sum Empty Empty

fromBool :: Bool -> RepBool
fromBool True  = Left ()
fromBool False = Right ()

toBool :: RepBool -> Bool
toBool (Left ())  = True
toBool (Right ()) = False

-- Suppose that we want to implement a generic datatype to binary encoder

-- gBiEncode . fromBool :: Bool -> [Bit]

-- So how should we go about implementing `gBiEncode`? 

biEncode' :: Sum Empty Empty -> [Bit]
biEncode' (Left ())  = [Zero]
biEncode' (Right ()) = [One]

-- Sum Zero (Product Zero (Sum Zero Zero))

-- Obviously this works for `RepBool` but does it work for 
-- any representable type? No! 

-- The problem is that by pattern matching we induce knowledge 
-- about the structure of the type, and in this case too much information.

-- What we can do is use type classes to convince the compiler that it can
-- recursively traverse the type structure without specifying the complete type
-- at the implementation side. 

class GenericEncode t where 
  gBiEncode :: t -> [Bit]

-- Upon calling `gBiEncode` with a specific type the compiler will proof
-- to us that it can provide an implementation for the given type.

-- The empty constructor contains no information

instance GenericEncode () where
  gBiEncode () = []

-- A choice can be encoded with a single bit

instance (GenericEncode a, GenericEncode b) => GenericEncode (Sum a b) where
  gBiEncode (Left l)  = Zero : gBiEncode l
  gBiEncode (Right l) = One  : gBiEncode l

-- And conjunction with concatenation

instance (GenericEncode a, GenericEncode b) => GenericEncode (Product a b) where
  gBiEncode (a,b)  = gBiEncode a ++ gBiEncode b

-- > gBiEncode (fromBool True)

-- Our current encoding allows us to encode all monomorphic data types, i.e.
-- datatypes without type parameters. 

-- We can represent monomorphic List types, like `List Int`:

type ListIntRep = Sum Empty (Product Int (List Int))

-- So what about the polymorphic data type `List a`?

-- To encode polymorphic data types we have extend the encoding with
-- an additional parameter `p` as a placeholder for the type parameter.

data    U         p = U                  -- Zero
    deriving Show

data    (:+:) f g p = L (f p) | R (g p)  -- Sum
    deriving Show

data    (:*:) f g p = (f p) :*: (g p)    -- Product
    deriving Show

-- But this is not enough, we also need pass the parameter along in 
-- recursive positions and provide a way the access the parameter

newtype Par1   p = Par1 p     -- gives access to parameter p
    deriving Show

newtype Rec1 f p = Rec1 (f p) -- a wrapper
    deriving Show

-- With this extended encoding we can represent `List a` as a binary sum of products

type ListRep = U :+: (Par1 :*: Rec1 List)

-- and define a projection from `List a` onto its more generic version `ListRep a`

fromList :: List a -> ListRep a
fromList Nil         = L U
fromList (List x xs) = R ((Par1 x) :*: (Rec1 xs))

-- and the converse

toList :: ListRep a -> List a
toList (L U)                        = Nil
toList (R ((Par1 x) :*: (Rec1 xs))) = List x xs

-- We now have all the prerequisites in place to re-implement the mapping
-- algorithm in terms `ListRep`. 

-- `gmap` will be the entry point that works on a concrete type, converts it into
-- the more generic representation, applies the more generic `gmap'`, and converts
-- the result back to the concrete type.

class GFunctor f where
  gmap :: (a -> b) -> f a -> f b

instance GFunctor List where
  gmap f = toList . gmap' f . fromList

class GFunctor' f where
  gmap' :: (a -> b) -> f a -> f b  

-- `gmap'` will work on the lifted representation 

instance GFunctor' U where
  gmap' _ U = U

instance GFunctor' Par1 where
  gmap' f (Par1 x) = Par1 (f x)

instance (GFunctor' f, GFunctor' g) => GFunctor' (f :+: g) where
  gmap' f (L x) = L (gmap' f x)
  gmap' f (R x) = R (gmap' f x)

instance (GFunctor' f, GFunctor' g) => GFunctor' (f :*: g) where
  gmap' f (x :*: y) = gmap' f x :*: gmap' f y

-- The recursive case is subtly different:

instance GFunctor f => GFunctor' (Rec1 f) where
  gmap' f (Rec1 xs) = Rec1 (gmap f xs)

-- since we encounter our conrete type again we recurse through 
-- the top-level entry point `gmap`

-- And it works!
-- > gmap (+1) exampleList

-- Taking it up a notch we can bundle the conversion functions into a type class

class Representable1 f where
  type Rep1 f :: * -> *

  fromRep1 :: f a -> (Rep1 f) a
  toRep1   :: (Rep1 f) a -> f a

-- and redefine `GFunctor` such that it provide a default implementation
-- by constraining the functor type to be representable

-- class GFunctor f where
--   gmap :: (a -> b) -> f a -> f b

--   default gmap :: (Representable1 f, GFunctor' (Rep1 f)) => (a -> b) -> f a -> f b
--   gmap f = toRep1 . gmap' f . fromRep1

instance Representable1 List where
  type Rep1 List = ListRep

  fromRep1 = fromList
  toRep1   = toList

-- and that's why deriving Functor works!

-- we don't need to write down the representations ourselves, let the
-- compiler do it for you with `DerivingGeneric`

-- inspect the generated binary sum of products
-- > :kind! Rep1 List

-- and reuse out of the box generic algorithms

instance GFoldable List
instance GFoldable Tree

collapseToList :: GFoldable f => f a -> [a]
collapseToList = gfoldMap (:[])

collapseToString :: (Show a, GFoldable f) => f a -> String
collapseToString = gfoldMap show

-- > [collapseToList exampleList, collapseToList exampleTree]
-- > [collapseToString exampleList, collapseToString exampleTree] 

-- :load JsEncode

-- -> Generic ADT to json encoder

{- 
Discussion:

There are many different approaches to datatype generic programming in Haskell
all with their own pros and cons. However, the main distinguishing feature is how
they view the structure of datatypes. 

"Comparing Libraries for Generic Programming in Haskell"[1] gives a thorough overview. 

Also, a more recent development shows (https://www.andres-loeh.de/TrueSumsOfProducts/)
that some functions are particularly difficult to implement in the encoding we've
presented. 

If you're concerned about the efficiency "Optimizing of Generic Programming through inlining" [2]
and "Optimizing Generics is Easy!" [3] go into more detail.

The approach we've explored is backed into GHC [4] and is fully described by
"A Generic Deriving Mechanism for Haskell" [5].

In case you want to know more about functors and 
such "Functors are Containers" [6] is a nice read.

https://people.cs.kuleuven.be/~tom.schrijvers/Research/talks/lhug1.pdf

[1] https://ai2-s2-pdfs.s3.amazonaws.com/51eb/0e70ed65f59141a17d29b3790aab08b12e58.pdf
[2] http://dreixel.net/research/pdf/ogpi.pdf
[3] https://pdfs.semanticscholar.org/ca99/c4f656626f00cff7bcdbd776d3c302ad4da4.pdf
[4] https://hackage.haskell.org/package/generic-deriving-1.11/
[5] http://dreixel.net/research/pdf/gdmh.pdf
[6] https://bartoszmilewski.com/2014/01/14/functors-are-containers/
-}

main :: IO ()
main = undefined
