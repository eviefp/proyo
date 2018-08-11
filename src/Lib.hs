module Lib
    ( someFunc
    ) where

import           Prelude (IO, putStrLn, (.), ($), undefined, id, fst, snd)

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

data Shop a b s t = Shop (s → a) (s → b → t)

instance Profunctor (Shop a b) where
  dimap (Shop s2a s2b2t) s2u t2v = Shop (s2a . s2u) (\s → t2v . s2b2t (s2u s))

instance Strong (Shop a b) where
  first (Shop s2a s2b2t) = Shop (\(a,_) → s2a a) (\(s,c) b → ((s2b2t s b), c))
  second x = undefined

data Lens s t a b = Lens
  { view   ∷ s → a
  , update ∷ (s, b) → t
  }

type LensP s t a b = ∀ p. Strong p ⇒ p a b → p s t

forward ∷ Lens s t a b → LensP s t a b
forward (Lens {..}) pab = dimap (second pab) (\s → (s,view s)) update

backwards ∷ LensP s t a b → Lens s t a b
backwards p
  = undefined
