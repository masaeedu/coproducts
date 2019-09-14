module Example1 where

import GHC.TypeLits

import Data.Proxy

import Coproducts (Coproduct(..), GFunctor(..), PlainFunctor(..), Flip(..), sum_map)

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

instance Coproduct (->) (Flip I a) (T a)
  where
  build (Flip (IFoo x)) = Foo x
  build (Flip (IBar x)) = Bar x

  match m (Bar x) = m $ Flip $ IBar x
  match m (Foo x) = m $ Flip $ IFoo x

instance GFunctor (->) (->) T where
  gfmap = sum_map (Proxy :: Proxy I)
