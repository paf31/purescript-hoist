module Data.Functor.Optic where

import Prelude (type (~>), class Functor, map, flip, (<<<), id, const, unit)

import Control.Comonad.Env (EnvT(..))
import Control.Comonad.Store (StoreT(..))
import Control.Comonad.Traced (TracedT(..))
import Data.Function (applyFlipped)
import Data.Functor.Day (Day, day, runDay)
import Data.Functor.Profunctor
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..))

-- | Isomorphisms between functors
type Iso s t a b = forall p. Profunctor p => p a b -> p s t

iso :: forall s t a b
     . (Functor s, Functor t, Functor a, Functor b)
    => s ~> a
    -> b ~> t
    -> Iso s t a b
iso f g pab = dimap g f pab

-- | `Blur` asserts that `s` and `t` are uniformly isomorphic to
-- | `Day a x` and `Day b x` for some functor `x`.
-- |
-- | Every `Blur` is also a `Hoist`.
type Blur s t a b = forall p. Convoluted p => p a b -> p s t

-- | `Pre` asserts that `s` and `t` are uniformly isomorphic to
-- | `Compose x a` and `Compose x b` for some functor `x`.
-- |
-- | Every `Pre` is also a `Hoist`.
type Pre s t a b = forall p. Precomposed p => p a b -> p s t

-- | `Post` asserts that `s` and `t` are uniformly isomorphic to
-- | `Compose a x` and `Compose b x` for some functor `x`.
-- |
-- | Every `Post` is also a `Hoist`.
type Post s t a b = forall p. Postcomposed p => p a b -> p s t

-- | `Hoist` asserts that natural transformations between `a` and `b` can
-- | be hoisted to natural transformations between `s` and `t`.
type Hoist s t a b = Natural a b -> Natural s t

hoistOf :: forall s t a b. Hoist s t a b -> a ~> b -> s ~> t
hoistOf blur f s = unNatural (blur (Natural f)) s

-- | `StoreT s w` is uniformly isomorphic to `Day (Store s)`.
stored :: forall w s t
        . Functor w
       => Blur (StoreT s w) (StoreT t w) (StoreT s Identity) (StoreT t Identity)
stored pab = dimap today tomorrow (convolveLeft pab) where
  today :: forall a. Day (StoreT t Identity) w a -> StoreT t w a
  today = runDay \f (StoreT (Tuple (Identity g) s)) w -> StoreT (Tuple (map (flip (f <<< g)) w) s)

  tomorrow :: forall a. StoreT s w a -> Day (StoreT s Identity) w a
  tomorrow (StoreT (Tuple w s)) = day applyFlipped (StoreT (Tuple (Identity id) s)) w

-- | `TracedT t w` is uniformly isomorphic to `Day (Traced t)`.
traced :: forall w s t
        . Functor w
       => Blur (TracedT s w) (TracedT t w) (TracedT s Identity) (TracedT t Identity)
traced pab = dimap today tomorrow (convolveLeft pab) where
  today :: forall a. Day (TracedT t Identity) w a -> TracedT t w a
  today = runDay \f (TracedT (Identity g)) w -> TracedT (map (flip (f <<< g)) w)

  tomorrow :: forall a. TracedT s w a -> Day (TracedT s Identity) w a
  tomorrow (TracedT w) = day applyFlipped (TracedT (Identity id)) w

-- | `EnvT t w` is uniformly isomorphic to `Day (Env t)`.
envied :: forall w s t
        . Functor w
       => Blur (EnvT s w) (EnvT t w) (EnvT s Identity) (EnvT t Identity)
envied pab = dimap today tomorrow (convolveLeft pab) where
  today :: forall a. Day (EnvT t Identity) w a -> EnvT t w a
  today = runDay \f (EnvT (Tuple e (Identity a))) w -> EnvT (Tuple e (map (f a) w))

  tomorrow :: forall a. EnvT s w a -> Day (EnvT s Identity) w a
  tomorrow (EnvT (Tuple e w)) = day (const id) (EnvT (Tuple e (Identity unit))) w
