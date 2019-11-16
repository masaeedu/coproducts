module Coproducts where

import Prelude hiding (id, (.))

import Data.Proxy

import Control.Category (Category)
import Control.Category

-- Some general category theory nonsense
type Hom c = c -> c -> *

-- Small functors
class (Category p, Category q) => GFunctor (p :: Hom c) (q :: Hom d) (f :: c -> d) | f -> p q
  where
  gfmap :: p a b -> q (f a) (f b)

type Endofunctor p = GFunctor p p

newtype PlainFunctor f a = PlainFunctor (f a)
  deriving Functor

instance Functor f => GFunctor (->) (->) (PlainFunctor f)
  where
  gfmap = fmap

-- Generalized product of an indexed family of objects
class Category p => Product (p :: Hom c) (idx :: i -> c) (x :: c)
  where
  extract   :: forall i. x `p` (idx i)
  decompose :: forall y. (forall i. y `p` idx i) -> y `p` x

newtype Op p a b = Op { runOp :: p b a }

instance Category p => Category (Op p)
  where
  id = Op id
  (Op f) . (Op g) = Op $ g . f

type Coproduct p idx x = Product (Op p) idx x

-- A newtype for flipping bifunctors around (useful below)
-- TODO: Replace `Flip` with `Symmetric` constraint
newtype Flip (t :: y -> x -> *) (a :: x) (b :: y)
  = Flip { runFlip :: t b a }

-- Getting a functor for a coproduct of functors
sum_map ::
  forall k f p a b.
  ( forall i. GFunctor p (->) (k i)
  , forall v. Coproduct (->) (Flip k v) (f v)
  ) =>
  Proxy k ->
  (a `p` b) -> f a -> f b
sum_map _ ab = match' $ inject' <<< gfmap ab
  where
  inject' :: forall i v. k i v -> f v
  inject' = runOp extract <<< Flip

  match' :: forall v y. (forall i. k i v -> y) -> f v -> y
  match' f = runOp $ decompose $ Op $ f <<< runFlip
