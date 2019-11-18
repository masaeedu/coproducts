module Coproducts where

import Prelude hiding (id, (.))

import Data.Proxy
import Data.Functor.Compose

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

instance GFunctor (Op (->)) (->) (Op (->) x)
  where
  gfmap f g = f <<< g

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

class Wat t
  where
  fwd ::
    GFunctor (Op (->)) (->) f =>
    t (f a) (f b) -> f (t a b)

  bwd ::
    GFunctor (Op (->)) (->) f =>
    f (t a b) -> t (f a) (f b)

newtype Biflip t c a b = Biflip { runBiflip :: t a b c }

-- }}}

-- INDEXED PRODUCTS {{{

class
  Category p =>
  Product
    (p :: Hom c)
    (key :: i -> *)
    (val :: i -> c)
    (prod :: c)
  where
  extract   :: Proxy key -> Proxy val -> key k -> prod `p` val k
  decompose :: Proxy key -> Proxy val -> (forall k. key k -> y `p` val k) -> y `p` prod

instance
  ( forall c. Wat (Biflip val c)
  , forall x y. Product (->) key (val x y) (prod x y)
  , GFunctor (Op (->)) (->) f
  ) =>
  Product
    (->)
    key
    (Compose f (val a b))
    (prod (f a) (f b))
  where
  extract _ _ k prod = Compose $ gfmap (Op Biflip) $ fwd $ Biflip $ extract key val k prod
    where
    key = Proxy @key
    val = Proxy @(val (f a) (f b))

  decompose _ _ m = decompose key val (\k y -> runBiflip $ bwd $ gfmap (Op runBiflip) $ getCompose $ m k y)
    where
    key = Proxy @key
    val = Proxy @(val (f a) (f b))

type Coproduct p key val x = Product (Op p) key val x

type UniversalProduct   p key val prod = forall t. Product   p key (val t) (prod t)
type UniversalCoproduct p key val sum  = forall t. Coproduct p key (val t) (sum t)

-- }}}

-- }}}

-- SINGLETONS {{{

class Singleton (f :: k -> *) (i :: k) | i -> f
  where
  reify :: Proxy i -> f i

-- }}}

-- A newtype for flipping bifunctors around
newtype Flip (t :: y -> x -> *) (a :: x) (b :: y)
  = Flip { runFlip :: t b a }

fold ::
  forall key val prod sum y.
  ( Product (->) key (Compose (Op (->) y) val) prod
  , Coproduct (->) key val sum
  ) =>
  Proxy key -> Proxy val -> prod -> sum -> y
fold key val cases = runOp $ decompose key val $ getCompose <<< flip (extract key Proxy) cases
