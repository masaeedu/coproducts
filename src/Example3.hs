module Example3 where

import Prelude hiding (id, (.))
import GHC.TypeLits

import Data.Proxy
import Data.Functor.Compose
import Data.Bifunctor

import Control.Category

import Coproducts (Product(..), Op(..), GFunctor(..), Wat(..), Biflip(..), Singleton(..), fold)

data Key (k :: Symbol)
  where
  SFst :: Key "Fst"
  SSnd :: Key "Snd"

-- APPROACH 1 {{{

-- instance Singleton Key "Fst"
--   where
--   reify _ = SFst

-- instance Singleton Key "Snd"
--   where
--   reify _ = SSnd

-- }}}

-- APPROACH 2 {{{

instance Singleton Key c
  where
  reify = _

-- }}}

data Val a b (k :: Symbol)
  where
  IFst :: { runIFst :: a } -> Val a b "Fst"
  ISnd :: { runISnd :: b } -> Val a b "Snd"

instance Product (Op (->)) Key (Val a b) (Either a b)
  where
  extract _ _ _ = Op $ \case
    IFst a -> Left a
    ISnd b -> Right b

  decompose _ _ m = Op $ \case
    Left a  -> runOp (m SFst) $ IFst a
    Right b -> runOp (m SSnd) $ ISnd b

instance Product (->) Key (Val a b) (a, b)
  where
  extract _ _ SFst (a, _) = IFst $ a
  extract _ _ SSnd (_, b) = ISnd $ b

  decompose _ _ m y = (runIFst $ m SFst y, runISnd $ m SSnd y)

instance Singleton Key k => Wat (Biflip Val k)
  where
  fwd (Biflip (IFst x)) = gfmap (Op (runIFst <<< runBiflip)) x
  fwd (Biflip (ISnd y)) = gfmap (Op (runISnd <<< runBiflip)) y

  bwd fa = case (reify (Proxy @k)) of
    SFst -> Biflip $ IFst $ gfmap (Op (Biflip <<< IFst)) $ fa
    SSnd -> Biflip $ ISnd $ gfmap (Op (Biflip <<< ISnd)) $ fa

either' :: forall a b x. (Op (->) x a, Op (->) x b) -> Either a b -> x
either' = fold (Proxy @Key) (Proxy @(Val a b))
