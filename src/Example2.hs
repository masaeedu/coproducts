module Example2 where

import Prelude hiding (id, (.))

import Unsafe.Coerce
import GHC.TypeLits

import Control.Category

import Data.Proxy
import qualified Data.Bifunctor as B

import Coproducts (Product(..), GFunctor(..), Flip(..), Op(..), sum_map)

-- A sum type that we would like to examine in a "deconstructed" form
data ProxyF t
  where
  Request ::              a' -> (a -> v) -> ProxyF '(a', a, b', b, m, r, v)
  Respond ::              (b' -> (b, v)) -> ProxyF '(a', a, b', b, m, r, v)
  M       :: Functor m => m v            -> ProxyF '(a', a, b', b, m, r, v)
  Pure    ::              r              -> ProxyF '(a', a, b', b, m, r, v)

-- Witness that `ProxyF` is a coproduct of the following types
type Request a' a          v = (a', a -> v)
type Respond      b' b     v = b' -> (b, v)
type M                 m   v = m v
type Pure                r   = r

-- The indexing family of the coproduct
data Index (c :: Symbol) t where
  IRequest ::              Request a' a v -> Index "Request" '(a', a, b', b, m, r, v)
  IRespond ::              Respond b' b v -> Index "Respond" '(a', a, b', b, m, r, v)
  IM       :: Functor m => M m v          -> Index "M"       '(a', a, b', b, m, r, v)
  IPure    ::              Pure r         -> Index "Pure"    '(a', a, b', b, m, r, v)

-- A product category with respect to which `ProxyF` is a functor
data ProxyArrow t1 t2
  where
  ProxyArrow ::
    (Functor m1 => Functor m2) =>
    (a'1 -> a'2)             ->
    (a2 -> a1)               ->
    (b'2 -> b'1)             ->
    (b1 -> b2)               ->
    (forall x. m1 x -> m2 x) ->
    (r1 -> r2)               ->
    (v1 -> v2)               ->
    ProxyArrow '(a'1, a1, b'1, b1, m1, r1, v1) '(a'2, a2, b'2, b2, m2, r2, v2)

instance Category ProxyArrow
  where
  id = unsafeCoerce $ ProxyArrow id id id id id id id
  (.) (ProxyArrow f1 f2 f3 f4 f5 f6 f7) (ProxyArrow g1 g2 g3 g4 g5 g6 g7) = ProxyArrow (f1 . g1) (g2 . f2) (g3 . f3) (f4 . g4) (f5 . g5) (f6 . g6) (f7 . g7)

-- Witness that the indexing type is a functor
instance GFunctor ProxyArrow (->) (Index c)
  where
  gfmap (ProxyArrow f1 f2 f3 f4 f5 f6 f7) = \case
    (IRequest (a', f)) -> IRequest (f1 a', f7 . f . f2)
    (IRespond f)       -> IRespond $ B.bimap f4 f7 . f . f3
    (IM m)             -> IM $ f5 $ fmap f7 $ m
    (IPure r)          -> IPure $ f6 $ r

-- Witness that `ProxyF` is a coproduct over the indexing family
instance Product (Op (->)) (Flip Index t) (ProxyF t)
  where
  extract = Op $ \case
    (Flip (IRequest (a', f))) -> Request a' f
    (Flip (IRespond f))       -> Respond f
    (Flip (IM mv))            -> M mv
    (Flip (IPure r))          -> Pure r

  decompose m = Op $ \case
    (Request a' f) -> runOp m $ Flip $ IRequest $ (a', f)
    (Respond f)    -> runOp m $ Flip $ IRespond $ f
    (M mv)         -> runOp m $ Flip $ IM       $ mv
    (Pure r)       -> runOp m $ Flip $ IPure    $ r

-- Witness the functor instance for `ProxyF`
instance GFunctor ProxyArrow (->) ProxyF
  where
  gfmap = sum_map (Proxy :: Proxy Index)

-- Try it out
test :: ProxyF '(a, b, c, d, e, Int, g)
test = id `gfmap` Pure 42
