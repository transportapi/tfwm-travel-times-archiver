{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Data.MonoidMap where

import           Control.DeepSeq (NFData)

import qualified Data.Aeson as A
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup (sconcat)

newtype MonoidMap k v = MonoidMap { getMap :: M.Map k v }
  deriving (Functor, Show, Eq, Ord, A.ToJSON, A.FromJSON, NFData)

instance (Ord k, Semigroup v) => Semigroup (MonoidMap k v) where
  MonoidMap m <> MonoidMap l = MonoidMap (M.unionWith (<>) m l)
  sconcat ms = MonoidMap (M.unionsWith (<>) $ map getMap $ NE.toList ms)

instance (Ord k, Monoid v) => Monoid (MonoidMap k v) where
  mempty = MonoidMap M.empty

type (~~>) = MonoidMap
infixr 5 ~~>

fromList :: Ord k => Monoid v => [(k, v)] -> MonoidMap k v
fromList kvs = MonoidMap $ M.fromListWith (<>) kvs

toList :: MonoidMap k v -> [(k, v)]
toList (MonoidMap mm) = M.toList mm

(~~>), singleton :: k -> v -> MonoidMap k v
singleton k v = MonoidMap $ M.singleton k v
(~~>) = singleton

lookup :: Ord k => k -> MonoidMap k v -> Maybe v
lookup k (MonoidMap m) = M.lookup k m
