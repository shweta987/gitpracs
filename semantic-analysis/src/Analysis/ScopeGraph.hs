{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, RecordWildCards, TypeApplications, TypeOperators #-}
module Analysis.ScopeGraph
( ScopeGraph(..)
, Ref (..)
, Decl(..)
, scopeGraph
) where

import           Analysis.Carrier.Env.Monovariant
import qualified Analysis.Carrier.Heap.Monovariant as A
import           Analysis.File
import           Analysis.FlowInsensitive
import           Analysis.Name
import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Carrier.Fail.WithLoc
import           Control.Carrier.Fresh.Strict
import           Control.Effect.State
import           Data.Foldable (fold)
import           Data.Function (fix)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Prelude hiding (fail)
import           Source.Span
import qualified System.Path as Path

data Decl = Decl
  { declSymbol :: Name
  , declPath   :: Path.AbsRelFile
  , declSpan   :: Span
  }
  deriving (Eq, Ord, Show)

data Ref = Ref
  { refPath :: Path.AbsRelFile
  , refSpan :: Span
  }
  deriving (Eq, Ord, Show)

type Addr = Name

newtype ScopeGraph = ScopeGraph { unScopeGraph :: Map.Map Decl (Set.Set Ref) }
  deriving (Eq, Ord, Show)

instance Semigroup ScopeGraph where
  ScopeGraph a <> ScopeGraph b = ScopeGraph (Map.unionWith (<>) a b)

instance Monoid ScopeGraph where
  mempty = ScopeGraph Map.empty

scopeGraph
  :: Ord (term Addr)
  => (forall sig m
     .  (Has (Reader Path.AbsRelFile) sig m, Has (Reader Span) sig m, MonadFail m)
     => (term Addr -> m ScopeGraph)
     -> (term Addr -> m ScopeGraph)
     )
  -> [File (term Addr)]
  -> (Heap ScopeGraph, [File (Either (Path.AbsRelFile, Span, String) ScopeGraph)])
scopeGraph eval
  = run
  . evalFresh 0
  . runHeap
  . traverse (runFile eval)

runFile
  :: ( Effect sig
     , Has Fresh sig m
     , Has (State (Heap ScopeGraph)) sig m
     , Ord (term Addr)
     )
  => (forall sig m
     .  (Has (Reader Path.AbsRelFile) sig m, Has (Reader Span) sig m, MonadFail m)
     => (term Addr -> m ScopeGraph)
     -> (term Addr -> m ScopeGraph)
     )
  -> File (term Addr)
  -> m (File (Either (Path.AbsRelFile, Span, String) ScopeGraph))
runFile eval file = traverse run file
  where run = runReader (filePath file)
            . runReader (fileSpan file)
            . runEnv
            . runFail
            . fmap fold
            . convergeTerm 0 (A.runHeap @Addr @ScopeGraph . fix (cacheTerm . eval))

-- scopeGraphAnalysis
--   :: ( Alternative m
--      , Has (Env Name) sig m
--      , Has (A.Heap Name ScopeGraph) sig m
--      , Has (Reader Path.AbsRelFile) sig m
--      , Has (Reader Span) sig m
--      )
--   => Analysis Name ScopeGraph m
-- scopeGraphAnalysis = Analysis{..}
--   where -- abstract eval name body = do
--         --   addr <- alloc @Addr name
--         --   A.assign @Addr @ScopeGraph name mempty
--         --   bind name addr (eval body)
--         -- apply _ f a = pure (f <> a)
--         record fields = do
--           fields' <- for fields $ \ (k, v) -> do
--             addr <- alloc k
--             path <- ask
--             span <- ask
--             let v' = ScopeGraph (Map.singleton (Decl k path span) mempty) <> v
--             (k, v') <$ A.assign @Addr addr v'
--           pure (foldMap snd fields')
--         _ ... m = pure (Just m)
