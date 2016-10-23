module Data.Functor.Hoist where

import Prelude
import Data.Either (Either(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Coproduct (Coproduct(..))
import Data.Functor.Product (Product(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))

-- | `Hoist` asserts that natural transformations between `a` and `b` can
-- | be hoisted to natural transformations between `s` and `t`.
-- |
-- | Usefully, the types line up with several standard functions from the core
-- | libraries:
-- |
-- | ```purescript
-- | Control.Monad.Free.hoistFree :: forall f g. Hoist (Free f)     (Free g)     f g
-- | Data.Coyoneda.hoistCoyoneda  :: forall f g. Hoist (Coyoneda f) (Coyoneda g) f g
-- | Data.Yoneda.hoistYoneda      :: forall f g. Hoist (Yoneda f)   (Yoneda g)   f g
-- | ```
-- |
-- | And from `purescript-mmorph`:
-- |
-- | ```purescript
-- | hoist :: forall f g t. MFunctor t => Hoist (t f) (t g) f g
-- | ```
-- |
-- | These functions compose like lenses, allowing us to hoist natural
-- | transformations over small parts of a structure:
-- |
-- | ```purescript
-- | hoistFree ⇜ right ⇜ first
-- |   :: forall f g x y
-- |    . Hoist (Free (Coproduct x (Product f y))) (Free (Coproduct x (Product g y))) f g
-- | ```
type Hoist s t a b = a ~> b -> s ~> t

-- | Compose `Hoist`ing functions.
-- |
-- | This is provided to help with type inference.
composeHoist
  :: forall a b c d e f
   . Hoist e f c d
  -> Hoist c d a b
  -> Hoist e f a b
composeHoist h1 h2 f = h1 (h2 f)

infixr 8 composeHoist as ⇜

-- | Hoist over the first component of a functor product.
first :: forall f g h. Hoist (Product f h) (Product g h) f g
first f (Product (Tuple fa ha)) = Product (Tuple (f fa) ha)

-- | Hoist over the second component of a functor product.
second :: forall f g h. Hoist (Product h f) (Product h g) f g
second f (Product (Tuple ha fa)) = Product (Tuple ha (f fa))

-- | Hoist over the first component of a functor coproduct.
left :: forall f g h. Hoist (Coproduct f h) (Coproduct g h) f g
left f (Coproduct (Left fa)) = Coproduct (Left (f fa))
left _ (Coproduct (Right ha)) = Coproduct (Right ha)

-- | Hoist over the second component of a functor coproduct.
right :: forall f g h. Hoist (Coproduct h f) (Coproduct h g) f g
right f (Coproduct (Right fa)) = Coproduct (Right (f fa))
right _ (Coproduct (Left ha)) = Coproduct (Left ha)

-- | Hoist over the inner function in a composition.
precomposed :: forall f g h. Functor h => Hoist (Compose h f) (Compose h g) f g
precomposed f fga = Compose (map f (unwrap fga))

-- | Hoist over the outer function in a composition.
composed :: forall f g h. Hoist (Compose f h) (Compose g h) f g
composed f fga = Compose (f (unwrap fga))
