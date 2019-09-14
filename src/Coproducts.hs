{-# LANGUAGE
    DataKinds
  , KindSignatures
  , PolyKinds
  , ConstraintKinds

  , GADTs

  , TypeFamilies
  , MultiParamTypeClasses
  , FunctionalDependencies

  , StandaloneDeriving
  , DerivingVia
  , DeriveFunctor
  , GeneralizedNewtypeDeriving

  , RankNTypes
  , QuantifiedConstraints

  , FlexibleContexts
  , FlexibleInstances

  , ViewPatterns
  , TypeOperators
  , ScopedTypeVariables
  , InstanceSigs
#-}

module Coproducts where

import Data.Proxy
import GHC.TypeLits

import Control.Category (Category)
import qualified Control.Category as C

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

-- Try it out
data T a = Foo (Maybe a) | Bar [a]
  deriving (Show)

data I (c :: Symbol) a
  where
  IFoo :: Maybe a -> I "Foo" a
  IBar :: [a]     -> I "Bar" a

deriving instance Functor (I c)
deriving via (PlainFunctor (I c)) instance GFunctor (->) (->) (I c)

instance Coproduct (->) (Flip I a) (T a)
  where
  build (Flip (IFoo x)) = Foo x
  build (Flip (IBar x)) = Bar x

  match m (Bar x) = m $ Flip $ IBar x
  match m (Foo x) = m $ Flip $ IFoo x

instance GFunctor (->) (->) T where
  gfmap = sum_map (Proxy :: Proxy I)

test :: T Int
test = (* 2) `gfmap` (Foo $ Just 5)

-- Test it out harder
-- A sum type that we would like to examine in a "deconstructed" form
data ProxyF t
  where
  Request :: a' -> (a -> v) -> ProxyF '(a', a, b', b, m, r, v)
  Respond :: (b' -> (b, v)) -> ProxyF '(a', a, b', b, m, r, v)
  M       :: m v            -> ProxyF '(a', a, b', b, m, r, v)
  Pure    :: r              -> ProxyF '(a', a, b', b, m, r, v)

-- Witness that Proxy is a coproduct of the following types
type Request a' a          v = (a', a -> v)
type Respond      b' b     v = b' -> (b, v)
type M                 m   v = m v
type Pure                r   = r

data Index (c :: Symbol) t where
  IRequest :: Request a' a v -> Index "Request" '(a', a, b', b, m, r, v)
  IRespond :: Respond b' b v -> Index "Respond" '(a', a, b', b, m, r, v)
  IM       :: M m v          -> Index "M"       '(a', a, b', b, m, r, v)
  IPure    :: Pure r         -> Index "Pure"    '(a', a, b', b, m, r, v)

instance Coproduct (->) (Flip Index t) (ProxyF t)
  where
  build (Flip (IRequest (a', f))) = Request a' f
  build (Flip (IRespond f))       = Respond f
  build (Flip (IM mv))            = M mv
  build (Flip (IPure r))          = Pure r

  match m (Request a' f) = m $ Flip $ IRequest $ (a', f)
  match m (Respond f)    = m $ Flip $ IRespond $ f
  match m (M mv)         = m $ Flip $ IM       $ mv
  match m (Pure r)       = m $ Flip $ IPure    $ r

-- TODO: Create a `ProxArrow t1 t2` indexed by '(a', a, b', b, m, r, v)
-- TODO: Witness `instance GFunctor ProxyArrow (->) (Index c)`
-- TODO: Witness `instance GFunctor ProxyArrow (->) ProxyF`
