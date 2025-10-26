{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

module Language.Explorer.Class
  (
    ExplorerM (..),
    Ref,
    Language,
    initialRef
  ) where

import Data.Tree (Tree)
import Control.Monad (Monad)

type Ref = Int

initialRef :: Ref
initialRef = 1

type Language p m c o = (Eq p, Eq o, Monad m, Monoid o)

class Language p m c o => ExplorerM e m p c o | e -> m p c o where
    execute :: p -> e -> m (e, o)
    executeAll :: [p] -> e -> m (e, o)
    jump :: Ref -> e -> m (Maybe e)
    revert :: Ref -> e -> m (Maybe e)
    deref :: Ref -> e -> m (Maybe c)
    config :: e -> m c
    currRef :: e -> m Ref
    toTree :: e -> m (Tree (Ref, c))
    leaves :: e -> m [(Ref, c)]
    getTrace :: e -> m [((Ref, c), (p, o), (Ref, c))]
