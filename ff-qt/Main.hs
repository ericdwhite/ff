{-# LANGUAGE NamedFieldPuns #-}
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
import Data.Maybe (fromJust, isJust)
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
import Foreign.C (CInt)
import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import Graphics.UI.Qtah.Widgets.QApplication (QApplication)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow)
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import Paths_ff_qt (version)
import RON.Storage.Backend (DocId (DocId))
import qualified RON.Storage.FS as Storage
import RON.Storage.FS
  ( CollectionDocId (CollectionDocId),
    runStorage,
    subscribeForever,
  )
import System.Environment (getArgs)

data UI = UI {window :: QMainWindow}

main :: IO ()
main = do
  path <- getDataDirOrFail
  storage <- Storage.newHandle path
  withApp $ \_ -> do
    setupApp
    ui@UI {window} <- setupUI
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

withApp :: (QApplication -> IO a) -> IO a
withApp = withScopedPtr $ do
  args <- getArgs
  QApplication.new args

setupApp :: IO ()
setupApp = do
  QCoreApplication.setOrganizationDomain "ff.cblp.su"
  QCoreApplication.setOrganizationName "ff"
  QCoreApplication.setApplicationName "ff"
  QCoreApplication.setApplicationVersion $ showVersion version

setupUI :: IO UI
setupUI = do
  window <- QMainWindow.new
  pure UI {window}

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
  -- provider = encodeUtf8 $ fromMaybe "" $ note_track >>= track_provider
  -- source = encodeUtf8 $ fromMaybe "" $ note_track >>= track_source
  -- externalId = encodeUtf8 $ fromMaybe "" $ note_track >>= track_externalId
  -- url = encodeUtf8 $ fromMaybe "" $ note_track >>= track_url
  undefined

toGregorianC :: Day -> (CInt, CInt, CInt)
toGregorianC day = (y, m, d)
  where
    (fromIntegral -> y, fromIntegral -> m, fromIntegral -> d) = toGregorian day
