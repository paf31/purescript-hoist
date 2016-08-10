## Module Data.Functor.Hoist

#### `Hoist`

``` purescript
type Hoist s t a b = a ~> b -> s ~> t
```

`Hoist` asserts that natural transformations between `a` and `b` can
be hoisted to natural transformations between `s` and `t`.

Usefully, the types line up with several standard functions from the core
libraries:

```purescript
Control.Monad.Free.hoistFree :: forall f g. Hoist (Free f)     (Free g)     f g
Data.Coyoneda.hoistCoyoneda  :: forall f g. Hoist (Coyoneda f) (Coyoneda g) f g
Data.Yoneda.hoistYoneda      :: forall f g. Hoist (Yoneda f)   (Yoneda g)   f g
```

And from `purescript-mmorph`:

```purescript
hoist :: forall f g t. MFunctor t => Hoist (t f) (t g) f g
```

These functions compose like lenses, allowing us to hoist natural
transformations over small parts of a structure:

```purescript
hoistFree ⇜ right ⇜ first
  :: forall f g x y
   . Hoist (Free (Coproduct x (Product f y))) (Free (Coproduct x (Product g y))) f g
```

#### `composeHoist`

``` purescript
composeHoist :: forall a b c d e f. (c ~> d -> e ~> f) -> (a ~> b -> c ~> d) -> a ~> b -> e ~> f
```

Compose `Hoist`ing functions.

This is provided to help with type inference.

#### `(⇜)`

``` purescript
infixr 8 composeHoist as ⇜
```

#### `first`

``` purescript
first :: forall f g h. Hoist (Product f h) (Product g h) f g
```

Hoist over the first component of a functor product.

#### `second`

``` purescript
second :: forall f g h. Hoist (Product h f) (Product h g) f g
```

Hoist over the second component of a functor product.

#### `left`

``` purescript
left :: forall f g h. Hoist (Coproduct f h) (Coproduct g h) f g
```

Hoist over the first component of a functor coproduct.

#### `right`

``` purescript
right :: forall f g h. Hoist (Coproduct h f) (Coproduct h g) f g
```

Hoist over the second component of a functor coproduct.

#### `precomposed`

``` purescript
precomposed :: forall f g h. Functor h => Hoist (Compose h f) (Compose h g) f g
```

Hoist over the inner function in a composition.

#### `composed`

``` purescript
composed :: forall f g h. Hoist (Compose f h) (Compose g h) f g
```

Hoist over the outer function in a composition.


