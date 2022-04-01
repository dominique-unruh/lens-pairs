{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( pair, unsafeCreate, lensFst, lensSnd, lensID, chain, get,
      Lens, Compatible, PhFst, PhSnd, PhPair, PhID, PhChain
    ) where

data Lens a m p = L (m -> a)

get :: Lens a m p -> m -> a
get (L g) m = g m

data CompatibilityWitness f g = UnsafeCompatibilityWitness

class Compatible f g where
  witness :: CompatibilityWitness f g

class SameLens f g

unsafeCreate :: (m -> a) -> Lens a m p
-- TODO Add an argument with a witness for p, this means only who "owns" p can unsafeCreate (?)
unsafeCreate get = L get

data PhFst = PhFst
lensFst :: Lens a (a,b) PhFst
lensFst = unsafeCreate (\(x,_) -> x)

data PhSnd = PhSnd
lensSnd :: Lens b (a,b) PhSnd
lensSnd = unsafeCreate (\(_,y) -> y)

data PhID = PhID
lensID :: Lens m m PhID
lensID = unsafeCreate id

data PhPair f g = PhPair
pair :: Compatible f g => Lens a m f -> Lens b m g -> Lens (a,b) m (PhPair f g)
pair (L fGet :: Lens a m f) (L gGet :: Lens b m g) = let !_ = (witness :: CompatibilityWitness f g) in unsafeCreate (\m -> (fGet m, gGet m))

data PhChain f g = PhChain
chain :: Lens b m f -> Lens a b g -> Lens a m (PhChain f g)
chain (L fGet) (L gGet) = unsafeCreate (gGet . fGet)

instance {-# INCOHERENT #-} Compatible PhFst PhSnd where
  witness = UnsafeCompatibilityWitness

instance {-# INCOHERENT #-} Compatible PhSnd PhFst where
  witness = UnsafeCompatibilityWitness
instance {-# INCOHERENT #-} Compatible f g => Compatible f (PhChain g h) where
  witness = UnsafeCompatibilityWitness
instance {-# INCOHERENT #-} (Compatible f h, Compatible g h) => Compatible (PhPair f g) h where
  witness = UnsafeCompatibilityWitness
instance {-# INCOHERENT #-} Compatible f g => Compatible (PhChain h f) (PhChain h g) where
  witness = UnsafeCompatibilityWitness

instance {-# INCOHERENT #-} SameLens PhID (PhPair PhFst PhSnd)
