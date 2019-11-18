module Example3 where

import Prelude hiding (id, (.))
import GHC.TypeLits

import Data.Functor.Compose

import Control.Category

import Coproducts (Product(..), Op(..), match, fold)

data Key (k :: Symbol)
  where
  SFst :: Key "Fst"
  SSnd :: Key "Snd"

data Val a b (k :: Symbol)
  where
  IFst :: { runIFst :: a } -> Val a b "Fst"
  ISnd :: { runISnd :: b } -> Val a b "Snd"

instance Product (Op (->)) Key (Val a b) (Either a b)
  where
  extract _ = Op $ \case
    IFst a -> Left a
    ISnd b -> Right b

  decompose m = Op $ \case
    Left a  -> runOp (m SFst) $ IFst a
    Right b -> runOp (m SSnd) $ ISnd b

instance Product (->) Key (Val a b) (a, b)
  where
  extract SFst (a, _) = IFst $ a
  extract SSnd (_, b) = ISnd $ b

  decompose m y = (runIFst $ m SFst y, runISnd $ m SSnd y)

instance Product (->) Key (Compose (Op (->) y) (Val a b)) (a -> y, b -> y)
  where
  extract SFst (a, _) = Compose $ Op $ a <<< runIFst
  extract SSnd (_, b) = Compose $ Op $ b <<< runISnd

  decompose m y =
    ( (<<< IFst) $ runOp $ getCompose $ m SFst y
    , (<<< ISnd) $ runOp $ getCompose $ m SSnd y
    )

either' :: forall a b x. (a -> x, b -> x) -> Either a b -> x
either' = fold @Key @(Val a b)
