{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module FF
    ( Agenda
    , DocId (..)
    , cmdAgenda
    , cmdDone
    , cmdNew
    ) where

import           CRDT.Cv (CvRDT)
import           CRDT.LamportClock (LamportTime (LamportTime), Pid,
                                    getRealLamportTime)
import           CRDT.LWW (LWW (LWW))
import qualified CRDT.LWW as LWW
import           Data.Aeson (FromJSON, ToJSON, ToJSONKey, Value (Array),
                             eitherDecode, encode, parseJSON, toJSON)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as BS
import           Data.Foldable (toList)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (Semigroup, sconcat, (<>))
import           Data.Semilattice (Semilattice)
import           Data.Text (Text)
import           Data.Traversable (for)
import           GHC.Exts (fromList)
import           System.Directory (createDirectoryIfMissing, listDirectory)
import           System.FilePath ((</>))

deriveJSON defaultOptions ''Pid

instance FromJSON a => FromJSON (LWW a) where
    parseJSON (Array a) = case toList a of
        [valueJ, timeJ, pidJ] -> LWW <$> parseJSON valueJ <*> parseTime
          where
            parseTime = LamportTime <$> parseJSON timeJ <*> parseJSON pidJ
        _ -> fail $ unwords
            ["expected array of 3 values, got", show $ length a, "values"]
    parseJSON v = typeMismatch "Array" v

instance ToJSON a => ToJSON (LWW a) where
    toJSON LWW{value, time = LamportTime time pid} =
        Array $ fromList [toJSON value, toJSON time, toJSON pid]

data Status = Active | Archived | Deleted
    deriving (Eq, Show)

deriveJSON defaultOptions ''Status

data Note = Note
    { status :: !(Maybe (LWW Status))
    , text   :: !(LWW Text)
    }
    deriving (Eq, Show)

instance Semigroup Note where
    Note alive1 text1 <> Note alive2 text2 =
        Note (alive1 <> alive2) (text1 <> text2)

instance Semilattice Note

deriveJSON defaultOptions ''Note

newtype DocId = DocId FilePath
    deriving (Eq, Ord, ToJSONKey)

-- TODO(cblp, 2018-01-05) deriving via GNTD
instance Show DocId where
    show (DocId path) = path

type NoteView = Text

type Agenda = Map DocId NoteView

loadDocument
    :: (CvRDT doc, FromJSON doc) => FilePath -> DocId -> IO (Maybe doc)
loadDocument dir (DocId doc) = do
    versionFiles <- listDirectory $ dir </> doc
    versions <- for versionFiles $ \version -> do
        let versionPath = dir </> doc </> version
        contents <- BS.readFile versionPath
        pure $
            either (error . ((versionPath ++ ": ") ++)) id $
            eitherDecode contents
    pure $ sconcat <$> nonEmpty versions

saveDocument
    :: (CvRDT doc, ToJSON doc)
    => FilePath -> DocId -> LamportTime -> doc -> IO ()
saveDocument dir (DocId docId) version doc = do
    let docDir = dir </> docId
    createDirectoryIfMissing True docDir
    let versionFile = docDir </> show version
    BS.writeFile versionFile $ encode doc

cmdAgenda :: FilePath -> IO Agenda
cmdAgenda dataDir = do
    let notesDir = dataDir </> "note"
    files <- listDirectory notesDir
    mnotes <- for files $ \name -> do
        let doc = DocId name
        note <- loadDocument notesDir doc
        let noteView = case note of
                Just Note{status = Nothing, text} ->
                    Just $ LWW.query text
                Just Note{status = Just status, text}
                    | LWW.query status == Active -> Just $ LWW.query text
                _ -> Nothing
        pure (doc, noteView)
    pure $ Map.fromList [(k, note) | (k, Just note) <- mnotes]

cmdNew :: FilePath -> Text -> IO DocId
cmdNew dataDir content = do
    let notesDir = dataDir </> "note"
    time <- getRealLamportTime
    let noteId = show time
    let docId = DocId noteId

    status <- Just <$> lwwIitial' Active
    text <- lwwIitial' content
    saveDocument notesDir docId time Note{status, text}

    pure docId

lwwIitial' :: a -> IO (LWW a)
lwwIitial' value = LWW value <$> getRealLamportTime

cmdDone :: FilePath -> DocId -> IO Text
cmdDone dataDir noteId = do
    let notesDir = dataDir </> "note"
    mnote <- loadDocument notesDir noteId
    let note =
            fromMaybe
                (error $ concat
                    [ "Can't load document "
                    , show noteId
                    , ". Where did you get this id?"
                    ])
                mnote
    time <- getRealLamportTime
    saveDocument
        notesDir
        noteId
        time
        note{status = Just LWW{value = Archived, time}}

    pure $ LWW.query $ text note
