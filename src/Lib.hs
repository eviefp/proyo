module Lib
    ( someFunc
    , Profunctor, dimap
    , Strong, first, second
    , Lens (..)
    , forward, backward, forward'
    ) where

import           Prelude (IO, putStrLn, (.), uncurry, fst, snd, curry)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-------------------------------------------------------------------------

class Profunctor p where
  dimap ∷ p a b → (c → a) → (b → d) → p c d

instance Profunctor (->) where
  dimap a2b c2a b2d = b2d . a2b . c2a


class Profunctor p ⇒ Strong p where
  first ∷ p a b → p (a, c) (b, c)
  second ∷ p a b → p (c, a) (c, b)

instance Strong (->) where
  first a2b ~(a,c)  = (a2b a, c)
  second a2b ~(c,a) = (c, a2b a)

-- Simple Lens as get/set
data Lens s t a b = Lens
  { view   ∷ s → a
  , update ∷ (s, b) → t
  }

-- Profunctor lens
type LensP s t a b = ∀ p. Strong p ⇒ p a b → p s t

-- Any Lens can be transformed to a profunctor lens
forward ∷ Lens s t a b → LensP s t a b
forward Lens {..} pab = dimap (second pab) (\s → (s, view s)) update

-- The other way around is not possible without choosing a `Strong`,
-- so we'll use the simplest one
data Shop a b s t = Shop (s → a) (s → b → t)

instance Profunctor (Shop a b) where
  dimap (Shop s2a s2b2t) s2u t2v = Shop (s2a . s2u) (\s → t2v . s2b2t (s2u s))

instance Strong (Shop a b) where
  first (Shop s2a s2b2t)  = Shop (s2a . fst) (\(s, c) b → ((s2b2t s b), c))
  second (Shop s2a s2b2t) = Shop (s2a . snd) (\(c, s) b → (c, (s2b2t s b)))

-- Trivially, we can transform a Lens to a Shop
-- (just like we could transform a Lens to a profunctor lens).
forward' ∷ Lens s t a b → Shop a b s t
forward' Lens {..} = Shop view (curry update)

-- Just as trivially, we can transform a Shop to a Lens.
backward ∷ Shop a b s t → Lens s t a b
backward (Shop v u) = Lens v (uncurry u)

-- So, Lens <-> Shop, and Shop is a specific (chosen) instance of profunctor lens
