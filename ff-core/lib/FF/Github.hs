{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module FF.Github
    ( getIssueViews
    , getOpenIssueSamples
    , sampleMap
    ) where

import           Control.Error (failWith)
import           Control.Monad.Except (ExceptT (..), liftIO, throwError,
                                       withExceptT)
import           Data.Foldable (toList)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (Day, UTCTime (..))
import           Data.Vector (Vector)
import           GitHub (Auth (OAuth), FetchCount (..), Issue (..),
                         IssueRepoMod, IssueState (..), Milestone (..),
                         URL (..), executeRequestMaybe, issueCreatedAt,
                         issueHtmlUrl, issueId, issueMilestone, issueState,
                         issueTitle, mkOwnerName, mkRepoName, stateAll,
                         stateOpen)
import           GitHub.Endpoints.Issues (issuesForRepoR)
import           System.Posix.ByteString (getEnv)
import           System.Process (readProcess)

import           FF (splitModes, takeSamples)
import           FF.Types (Limit, ModeMap, NoteView (..), Sample (..),
                           Status (..), Tracked (..))

getIssues
    :: Maybe Text
    -> Maybe Limit
    -> IssueRepoMod
    -> ExceptT Text IO (Text, Vector Issue)
getIssues mAddress mlimit issueState = do
    address <- case mAddress of
        Just address -> pure address
        Nothing -> do
            packed <- liftIO $ Text.pack <$>
                readProcess "git" ["remote", "get-url", "--push", "origin"] ""
            failWith "Sorry, only github repository expected." $
                Text.stripPrefix "https://github.com/" packed
                >>= Text.stripSuffix ".git\n"
    (owner, repo) <- case Text.splitOn "/" address of
        [owner, repo] | not $ Text.null owner, not $ Text.null repo ->
            pure (owner, repo)
        _ -> throwError $
            "Something is wrong with " <> address <>
            ". Please, check correctness of input. Right format is OWNER/REPO"
    auth <-
        liftIO (getEnv "FF_GITHUB_TOKEN") >>= \case
            Nothing    -> pure Nothing
            Just token -> pure $ Just $ OAuth token
    response <- withExceptT (Text.pack . show) $ ExceptT $
        executeRequestMaybe auth $ issuesForRepoR
            (mkOwnerName owner)
            (mkRepoName repo)
            issueState
            (maybe FetchAll (FetchAtLeast . fromIntegral) mlimit)
    pure (address, response)

getOpenIssueSamples
    :: Maybe Text
    -> Maybe Limit
    -> Day
    -> ExceptT Text IO (ModeMap Sample)
getOpenIssueSamples mAddress mlimit today = do
    (address, issues) <- getIssues mAddress mlimit stateOpen
    pure $ sampleMap address mlimit today issues

getIssueViews
    :: Maybe Text
    -> Maybe Limit
    -> ExceptT Text IO [NoteView]
getIssueViews mAddress mlimit = do
    (address, issues) <- getIssues mAddress mlimit stateAll
    pure $ noteViewList address mlimit issues

sampleMap
    :: Foldable t => Text -> Maybe Limit -> Day -> t Issue -> ModeMap Sample
sampleMap address mlimit today issues =
    takeSamples mlimit
    . splitModes today
    . map (issueToNoteView address)
    . maybe id (take . fromIntegral) mlimit
    $ toList issues

noteViewList :: Foldable t => Text -> Maybe Limit -> t Issue -> [NoteView]
noteViewList address mlimit issues =
    map (issueToNoteView address)
    . maybe id (take . fromIntegral) mlimit
    $ toList issues

issueToNoteView :: Text -> Issue -> NoteView
issueToNoteView address Issue{..} = NoteView
    { nid     = Nothing
    , status  = toStatus issueState
    , text    = issueTitle
    , start   = utctDay issueCreatedAt
    , end
    , tracked = Just Tracked
        { trackedProvider   = "github"
        , trackedSource     = address
        , trackedExternalId
        , trackedUrl
        }
    }
  where
    trackedExternalId = Text.pack $ show issueNumber
    trackedUrl = case issueHtmlUrl of
        Just (URL url) -> url
        Nothing        ->
            "https://github.com/" <> address <> "/issues/" <> trackedExternalId
    end = case issueMilestone of
        Just Milestone{milestoneDueOn = Just UTCTime{utctDay}} -> Just utctDay
        _                                                      -> Nothing

toStatus :: IssueState -> Status
toStatus = \case
    StateOpen   -> Active
    StateClosed -> Archived
