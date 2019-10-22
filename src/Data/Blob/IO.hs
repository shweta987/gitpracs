{-# LANGUAGE RankNTypes #-}

-- | These are primitive file IO methods for use in ghci and as internal functions.
-- Instead of using these, consider if you can use the Files DSL instead.
module Data.Blob.IO
  ( readBlobFromFile
  , readBlobFromFile'
  , readBlobFromPath
  , demandBlobFromPath
  , readBlobsFromDir
  , readFilePair
  ) where

import Prologue

import qualified Control.Concurrent.Async as Async
import           Data.Blob
import qualified Data.ByteString as B
import           Data.Language
import           Semantic.IO
import qualified Source.Source as Source
import qualified System.Path as Path
import qualified System.Path.PartClass as Path.Class

-- | Read a utf8-encoded file to a 'Blob'.
readBlobFromFile :: forall m. MonadIO m => File -> m (Maybe Blob)
readBlobFromFile (File "/dev/null" _) = pure Nothing
readBlobFromFile (File path language) = do
  raw <- liftIO $ B.readFile path
  pure . Just . sourceBlob path language . Source.fromUTF8 $ raw

readBlobFromPath :: (MonadIO m, Path.Class.AbsRel ar) => Path.File ar -> m (Maybe Blob)
readBlobFromPath p
  | p == Path.file "/dev/null" = pure Nothing
  | otherwise = do
      raw <- liftIO . B.readFile . Path.toString $ p
      pure . Just $ Blob (Source.fromUTF8 raw) (fileForTypedPath p) mempty

demandBlobFromPath :: (MonadIO m, MonadFail m, Path.Class.AbsRel ar) => Path.File ar -> m Blob
demandBlobFromPath p = readBlobFromPath p >>= maybeM perish
  where perish = Prologue.fail ("Cannot read from path " <> Path.toString p <> ", file not found or language not supported")

-- | Read a utf8-encoded file to a 'Blob', raising an IOError if it can't be found.
readBlobFromFile' :: MonadIO m => File -> m Blob
readBlobFromFile' file = do
  maybeFile <- readBlobFromFile file
  maybeM (Prelude.fail ("cannot read '" <> show file <> "', file not found or language not supported.")) maybeFile

-- | Read all blobs in the directory with Language.supportedExts.
readBlobsFromDir :: MonadIO m => Path.AbsRelDir -> m [Blob]
readBlobsFromDir path = liftIO . fmap catMaybes $
  findFilesInDir path supportedExts mempty >>= Async.mapConcurrently (readBlobFromFile . fileForTypedPath)

readFilePair :: MonadIO m => File -> File -> m BlobPair
readFilePair a b = do
  before <- readBlobFromFile a
  after  <- readBlobFromFile b
  liftIO $ maybeBlobPair before after
