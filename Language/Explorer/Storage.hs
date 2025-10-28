{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, ExplicitForAll #-}


module Language.Explorer.Storage where 

import Control.Monad.State

type Ref = Int

class Storage m p c o s where 
    new :: c -> StateT (s p c o) m Ref
    query :: Ref -> StateT (s p c o) m (Maybe c)
    link :: Ref -> (p, o) -> Ref -> StateT (s p c o) m ()
    unlink :: Ref -> Ref -> StateT (s p c o) m ()
    -- All paths from r1 -> ?r2. If r2 is not specified, return all outgoing paths from r1.
    paths :: Ref -> Maybe Ref -> StateT (s p c o) m [(Ref, (p, o), Ref)] 
