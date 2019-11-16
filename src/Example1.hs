module Example1 where

import GHC.TypeLits

import Data.Proxy

import Coproducts (Product(..), GFunctor(..), PlainFunctor(..), Flip(..), Op(..), sum_map)

data T a
  = Foo (Maybe a)
  | Bar [a]
  deriving (Show)

data Index (c :: Symbol) a
  where
  IFoo :: Maybe a -> Index "Foo" a
  IBar :: [a]     -> Index "Bar" a
deriving instance Functor (Index c)
deriving via (PlainFunctor (Index c)) instance GFunctor (->) (->) (Index c)

instance Product (Op (->)) (Flip Index a) (T a)
  where
  extract = Op $ \case
    (Flip (IFoo x)) -> Foo x
    (Flip (IBar x)) -> Bar x

  decompose m = Op $ \case
    (Bar x) -> runOp m $ Flip $ IBar x
    (Foo x) -> runOp m $ Flip $ IFoo x

instance GFunctor (->) (->) T where
  gfmap = sum_map (Proxy :: Proxy Index)
