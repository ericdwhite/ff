{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main
  ( main,
  )
where

import Control.Concurrent (forkIO)
import Data.Foldable (for_)
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, toGregorian)
import Data.Typeable (cast)
import Data.Version (showVersion)
import FF
  ( fromRgaM,
    getDataDir,
    loadTasks,
    noDataDirectoryMessage,
  )
import FF.Config (loadConfig)
import FF.Types
  ( Entity (Entity, entityId, entityVal),
    Note (Note, note_end, note_start, note_status, note_text, note_track),
    NoteId,
    NoteStatus (TaskStatus),
    NoteView (note),
    Status (Active),
    Track (track_externalId, track_provider, track_source, track_url),
    loadNote,
  )
import Foreign (Ptr)
import Foreign.C (CInt)
import Foreign.Hoppy.Runtime (withScopedPtr)
import Foreign.StablePtr (newStablePtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow)
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import qualified Language.C.Inline.Cpp as Cpp
import Paths_ff_qt (version)
import RON.Storage.Backend (DocId (DocId))
import qualified RON.Storage.FS as Storage
import RON.Storage.FS
  ( CollectionDocId (CollectionDocId),
    runStorage,
    subscribeForever,
  )

data UI = UI {window :: QMainWindow}

main :: IO ()
main = do
  let version' = encodeUtf8 . Text.pack $ showVersion version
  path <- getDataDirOrFail
  storage <- Storage.newHandle path
  storagePtr <- newStablePtr storage
  withApp $ \app -> do
    ui@UI {window} <- setupUI
    --   int argc = 0;
    --   char argv0[] = "ff-qt";
    --   char * argv[] = {argv0, NULL};

    --   auto app = new QApplication(argc, argv);
    --   app->setOrganizationDomain("ff.cblp.su");
    --   app->setOrganizationName("ff");
    --   app->setApplicationName("ff");
    --   app->setApplicationVersion(QString::fromStdString($bs-cstr:version'));
    QWidget.show window
    -- load current data to the view, asynchronously
    _ <-
      forkIO $ do
        activeTasks <- runStorage storage (loadTasks False)
        for_ activeTasks $ upsertTask ui . note
    -- update the view with future changes
    _ <- forkIO $ subscribeForever storage $ upsertDocument storage ui
    -- run UI
    QCoreApplication.exec

withApp = withScopedPtr $ do
  args <- getArgs
  QApplication.new args

setupUI = do
  pure UI {}

getDataDirOrFail :: IO FilePath
getDataDirOrFail = do
  cfg <- loadConfig
  dataDir <- getDataDir cfg
  case dataDir of
    Nothing -> fail noDataDirectoryMessage
    Just path -> pure path

upsertDocument :: Storage.Handle -> UI -> CollectionDocId -> IO ()
upsertDocument storage mainWindow (CollectionDocId docid) = case docid of
  (cast -> Just (noteId :: NoteId)) -> do
    note <- runStorage storage $ loadNote noteId
    upsertTask mainWindow note
  _ -> pure ()

upsertTask :: UI -> Entity Note -> IO ()
upsertTask mainWindow Entity {entityId = DocId nid, entityVal = note} = do
  let nid' = encodeUtf8 $ Text.pack nid
      isActive = note_status == Just (TaskStatus Active)
      Note {note_text, note_start, note_end, note_track, note_status} = note
      noteText = fromRgaM note_text
      text = encodeUtf8 $ Text.pack noteText
      (startYear, startMonth, startDay) = toGregorianC $ fromJust note_start
      (endYear, endMonth, endDay) = maybe (0, 0, 0) toGregorianC note_end
      isTracking = isJust note_track
      provider = encodeUtf8 $ fromMaybe "" $ note_track >>= track_provider
      source = encodeUtf8 $ fromMaybe "" $ note_track >>= track_source
      externalId = encodeUtf8 $ fromMaybe "" $ note_track >>= track_externalId
      url = encodeUtf8 $ fromMaybe "" $ note_track >>= track_url
  -- [Cpp.block| void {
  --   $(MainWindow * mainWindow)->upsertTask({
  --     .id = $bs-cstr:nid',
  --     .isActive = $(bool isActive),
  --     .text = $bs-cstr:text,
  --     .start = {$(int startYear), $(int startMonth), $(int startDay)},
  --     .end   = {$(int   endYear), $(int   endMonth), $(int   endDay)},
  --     .isTracking = $(bool isTracking),
  --     .track = {
  --       .provider   = $bs-cstr:provider,
  --       .source     = $bs-cstr:source,
  --       .externalId = $bs-cstr:externalId,
  --       .url        = $bs-cstr:url,
  --     },
  --   });
  -- } |]
  _

toGregorianC :: Day -> (CInt, CInt, CInt)
toGregorianC day = (y, m, d)
  where
    (fromIntegral -> y, fromIntegral -> m, fromIntegral -> d) = toGregorian day
