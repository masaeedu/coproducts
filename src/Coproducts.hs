module Coproducts where

import Unsafe.Coerce
import Data.Proxy
import GHC.TypeLits

import Control.Category (Category)
import qualified Control.Category as C

import qualified Data.Bifunctor as B

-- Some general category theory nonsense
type Hom c = c -> c -> *

-- Small categories
class GCategory (p :: Hom c)
  where
  gidentity :: forall a. p a a
  (<<<) :: forall a b c. p b c -> p a b -> p a c

newtype PlainCategory p a b = PlainCategory (p a b)
  deriving Category

instance Category c => GCategory (PlainCategory c)
  where
  gidentity = C.id
  (<<<) = (C.<<<)

deriving via (PlainCategory (->)) instance GCategory (->)

-- Small functors
class (GCategory p, GCategory q) => GFunctor (p :: Hom c) (q :: Hom d) (f :: c -> d) | f -> p q
  where
  gfmap :: p a b -> q (f a) (f b)

type Endofunctor p = GFunctor p p

newtype PlainFunctor f a = PlainFunctor (f a)
  deriving Functor

instance Functor f => GFunctor (->) (->) (PlainFunctor f)
  where
  gfmap = fmap

-- Generalized coproduct of an indexed family of objects
class GCategory p => Coproduct (p :: Hom c) (idx :: i -> c) (x :: c)
  where
  build :: forall i. idx i `p` x
  match :: forall y. (forall i. idx i `p` y) -> x `p` y

-- A newtype for flipping bifunctors around (useful below)
newtype Flip (t :: y -> x -> *) (a :: x) (b :: y) = Flip { runFlip :: t b a }

-- Getting a functor for a coproduct of functors
sum_map ::
  forall k f p a b.
  ( forall i. GFunctor p (->) (k i)
  , forall v. Coproduct (->) (Flip k v) (f v)
  ) =>
  Proxy k ->
  (a `p` b) -> f a -> f b
sum_map _ ab = match' $ build' <<< gfmap ab
  where
  -- irrelevant, but interestingly the two commented out wrong type annotations below blow up the compiler
  -- build' :: forall i v. k i v -> f i
  -- build' :: forall i v. k v i -> f v
  build' :: forall i v. k i v -> f v
  build' = build <<< Flip

  match' :: forall v y. (forall i. k i v -> y) -> f v -> y
  match' f = match $ f <<< runFlip
-- TODO: Replace `Flip` with `Symmetric` constraint
