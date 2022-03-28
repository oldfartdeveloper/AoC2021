module Utils where

import Prelude

import Data.Maybe (Maybe, fromJust)
import Partial.Unsafe (unsafePartial)

unsafeJust :: forall a. Maybe a -> a
unsafeJust x = unsafePartial $ fromJust x
