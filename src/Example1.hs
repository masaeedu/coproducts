module Example1 where

import GHC.TypeLits

import Data.Proxy

import Coproducts (Product(..), GFunctor(..), PlainFunctor(..), Flip(..), Op(..), sum_map)

data T a
  = Foo (Maybe a)
  | Bar [a]
  deriving (Show)

data I (c :: Symbol) a
  where
  IFoo :: Maybe a -> I "Foo" a
  IBar :: [a]     -> I "Bar" a
deriving instance Functor (I c)
deriving via (PlainFunctor (I c)) instance GFunctor (->) (->) (I c)

instance Product (Op (->)) (Flip I a) (T a)
  where
  extract = Op $ \case
    (Flip (IFoo x)) -> Foo x
    (Flip (IBar x)) -> Bar x

  decompose m = Op $ \case
    (Bar x) -> runOp m $ Flip $ IBar x
    (Foo x) -> runOp m $ Flip $ IFoo x

instance GFunctor (->) (->) T where
  gfmap = sum_map (Proxy :: Proxy I)
