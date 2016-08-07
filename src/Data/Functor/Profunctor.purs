module Data.Functor.Profunctor where

import Prelude

import Data.Functor.Day (Day, day, runDay)

-- | Profunctors in the functor category
class Profunctor p where
  dimap :: forall a b c d
         . (Functor a, Functor b, Functor c, Functor d)
        => b ~> d
        -> c ~> a
        -> p a b
        -> p c d

-- | Composition of functors
data Compose f g a = Compose (f (g a))

runCompose :: forall f g a. Compose f g a -> f (g a)
runCompose (Compose fga) = fga

-- | Profunctors which support lifting over compositions (on the left).
-- |
-- | See "Monad Transformer Lenses" for details at
-- | <https://www.youtube.com/watch?v=Bxcz23GOJqc>.
class Profunctor p <= Precomposed p where
  precompose :: forall a b c
              . (Functor a, Functor b, Functor c)
             => p a b
             -> p (Compose c a) (Compose c b)

-- | Profunctors which support lifting over compositions (on the right).
class Profunctor p <= Postcomposed p where
  postcompose :: forall a b c
               . (Functor a, Functor b, Functor c)
              => p a b
              -> p (Compose a c) (Compose b c)

-- | Profunctors which support lifting over `Day`.
class Profunctor p <= Convoluted p where
  convolveLeft :: forall a b c
                . (Functor a, Functor b, Functor c)
               => p a b
               -> p (Day a c) (Day b c)
  convolveRight :: forall a b c
                 . (Functor a, Functor b, Functor c)
                => p a b
                -> p (Day c a) (Day c b)

-- | Natural transformations, wrapped in a newtype to provide instances for
-- | `Profunctor` and `Convoluted`.
newtype Natural f g = Natural (f ~> g)

unNatural :: forall f g. Natural f g -> f ~> g
unNatural (Natural f) = f

instance profunctorNatural :: Profunctor Natural where
  dimap f g h = Natural \a -> f (unNatural h (g a))

instance precomposedNatural :: Precomposed Natural where
  precompose n = Natural (Compose <<< map (unNatural n) <<< runCompose)

instance postcomposedNatural :: Postcomposed Natural where
  postcompose n = Natural (Compose <<< unNatural n <<< runCompose)

instance convolutedNatural :: Convoluted Natural where
  convolveLeft n = Natural (runDay \f x y -> day f (unNatural n x) y)
  convolveRight n = Natural (runDay \f x y -> day f x (unNatural n y))
