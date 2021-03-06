{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This carrier interprets the Sketch effect, keeping track of
-- the current scope and in-progress graph internally.
module Control.Carrier.Sketch.Fresh
  ( SketchC (..)
  , runSketch
  , module Control.Effect.Sketch
  ) where

import           Analysis.Name (Name)
import qualified Analysis.Name as Name
import           Control.Algebra
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.State.Strict
import           Control.Carrier.Reader
import           Control.Effect.Sketch
import           Control.Monad.IO.Class
import           Data.Bifunctor
import           Data.Module
import           Data.ScopeGraph (ScopeGraph)
import qualified Data.ScopeGraph as ScopeGraph
import           Data.Semilattice.Lower
import           GHC.Records
import           Source.Span
import qualified System.Path as Path

-- | The state type used to keep track of the in-progress graph and
-- positional/contextual information. The name "sketchbook" is meant
-- to invoke an in-progress, concealed work, as well as the
-- "sketching" of a graph.
data Sketchbook = Sketchbook
  { sGraph        :: ScopeGraph Name
  , sCurrentScope :: Name
  } deriving (Eq, Show)

instance Lower Sketchbook where
  lowerBound =
    let
      initialGraph = ScopeGraph.insertScope n lowerBound lowerBound
      n = Name.nameI 0
    in
      Sketchbook initialGraph n

newtype SketchC address m a = SketchC (StateC Sketchbook (FreshC m) a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Effect sig, Algebra sig m) => Algebra (SketchEff :+: Reader Name :+: Fresh :+: sig) (SketchC Name m) where
  alg (L (Declare n props k)) = do
    Sketchbook old current <- SketchC (get @Sketchbook)
    let (new, _pos) =
          ScopeGraph.declare
          (ScopeGraph.Declaration n)
          (lowerBound @ModuleInfo)
          (relation props)
          ScopeGraph.Public
          (lowerBound @Span)
          (getField @"kind" @DeclProperties props)
          (associatedScope props)
          current
          old
    SketchC (put (Sketchbook new current))
    k ()
  alg (L (Reference n decl _props k)) = do
    Sketchbook old current <- SketchC (get @Sketchbook)
    let new =
          ScopeGraph.reference
          (ScopeGraph.Reference (Name.name n))
          (lowerBound @ModuleInfo)
          (lowerBound @Span)
          ScopeGraph.Identifier
          (ScopeGraph.Declaration (Name.name decl))
          current
          old
    SketchC (put (Sketchbook new current))
    k ()
  alg (L (NewScope edges k)) = do
    Sketchbook old current <- SketchC get
    name <- SketchC Name.gensym
    let new = ScopeGraph.newScope name edges old
    SketchC (put (Sketchbook new current))
    k name
  alg (R (L a)) = case a of
    Ask k -> SketchC (gets sCurrentScope) >>= k
    Local fn go k -> do
      initial@(Sketchbook s oldScope) <- SketchC get
      let newScope = fn oldScope
      SketchC (put (Sketchbook s newScope))
      result <- go
      SketchC (put initial)
      k result

  alg (R (R (L a))) = send (handleCoercible a)
  alg (R (R (R a))) = send (handleCoercible a)

runSketch ::
  (Functor m)
  => Maybe Path.AbsRelFile
  -> SketchC Name m a
  -> m (ScopeGraph Name, a)
runSketch _rootpath (SketchC go)
  = evalFresh 1
  . fmap (first sGraph)
  . runState lowerBound
  $ go
