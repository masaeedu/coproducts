module Example1 where
--
-- import GHC.TypeLits
--
-- import Coproducts (Product(..), GFunctor(..), PlainFunctor(..), Flip(..), Op(..))
--
-- data T a
--   = Foo (Maybe a)
--   | Bar [a]
--   deriving (Show)
--
-- data Key (c :: Symbol)
--   where
--   SFoo :: Key "Foo"
--   SBar :: Key "Bar"
--
-- data Index (c :: Symbol) a
--   where
--   IFoo :: Maybe a -> Index "Foo" a
--   IBar :: [a]     -> Index "Bar" a
-- deriving instance Functor (Index c)
-- deriving via (PlainFunctor (Index c)) instance GFunctor (->) (->) (Index c)
--
-- instance Product (Op (->)) Key (Flip Index a) (T a)
--   where
--   extract _ = Op $ \case
--     Flip (IFoo x) -> Foo x
--     Flip (IBar x) -> Bar x
--
--   decompose m = Op $ \case
--     Bar x -> runOp (m SBar) $ Flip $ IBar x
--     Foo x -> runOp (m SFoo) $ Flip $ IFoo x
--
-- -- instance GFunctor (->) (->) T where
-- --   gfmap = sum_map (Proxy :: Proxy Index)
