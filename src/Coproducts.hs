{-# LANGUAGE ImpredicativeTypes, AllowAmbiguousTypes #-}

module Coproducts where

import Prelude hiding (id, (.))

import Data.Proxy
import Data.Functor.Compose
import Data.Functor.Const

import Control.Category (Category)
import Control.Category

-- CATEGORY THEORY {{{

type Hom c = c -> c -> *

-- OPPOSITE CATEGORIES {{{

newtype Op p a b = Op { runOp :: p b a }

instance Category p => Category (Op p)
  where
  id = Op id
  (Op f) . (Op g) = Op $ g . f

-- }}}

-- FUNCTORS {{{

class (Category p, Category q) => GFunctor (p :: Hom c) (q :: Hom d) (f :: c -> d) | f -> p q
  where
  gfmap :: p a b -> q (f a) (f b)

type Endofunctor p = GFunctor p p

newtype PlainFunctor f a = PlainFunctor (f a)
  deriving Functor

instance Functor f => GFunctor (->) (->) (PlainFunctor f)
  where
  gfmap = fmap

-- }}}

-- INDEXED PRODUCTS {{{

class
  Category p =>
  Product
    (p :: Hom c)
    (key :: i -> *)
    (val :: i -> c)
    (x :: c)
  where
  extract   :: key k -> x `p` val k
  decompose :: (forall k. key k -> y `p` val k) -> y `p` x

type Coproduct p key val x = Product (Op p) key val x

type UniversalProduct   p key val prod = forall t. Product   p key (val t) (prod t)
type UniversalCoproduct p key val sum  = forall t. Coproduct p key (val t) (sum t)

inject :: Coproduct p key val x => key k -> val k `p` x
inject s = runOp $ extract s

match :: Coproduct p key val x => (forall k. key k -> val k `p` y) -> x `p` y
match cases = runOp $ decompose $ Op <<< cases

extract' :: forall key val p prod k.
  Product p key val prod =>
  key k -> prod `p` val k
extract' = extract

decompose' :: forall key val p prod y.
  Product p key val prod =>
  (forall k. key k -> y `p` val k) -> y `p` prod
decompose' = decompose

-- }}}

-- }}}

-- A newtype for flipping bifunctors around
newtype Flip (t :: y -> x -> *) (a :: x) (b :: y)
  = Flip { runFlip :: t b a }

fold ::
  forall key val prod sum y.
  ( Product (->) key (Compose (Op (->) y) val) prod
  , Coproduct (->) key val sum
  ) =>
  prod -> sum -> y
fold cases = runOp $ decompose' @key @val $ getCompose <<< flip extract cases
