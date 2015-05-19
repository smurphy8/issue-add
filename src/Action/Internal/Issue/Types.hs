

{- |
Module      :  Action.Internal.Issue.Types
Description :  Types needed for issues
Copyright   :  Plow Technologies LLC
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

Both HIssue and HNewIssue are stored here,
the 'H' represents the fact that they render to headings in org-mode

-}


module Action.Internal.Issue.Types where
import           Data.Text                   (Text)
import           Data.Time                   (UTCTime)
import qualified Github.Issues               as Github
-- Locacl
import           Action.Internal.Label.Types (RepoLabel (..))

-- | DataTypes


-- | It might seem excessive to write types down to model each thing... but I just
-- want the flexibility to add meta info and other helpful things w/o depending on the
-- github api.
-- the 'H' is for 'Heading'
-- the 'OrgIssue' is for orgmode there are close ties to the data types used in 'Github'

data OrgIssueOwnerType = GithubUser | GithubOrganization
  deriving (Eq, Ord, Show)

data OrgIssueOwner = OrgIssueOwner {
                          _ownerType       :: OrgIssueOwnerType
                        , _avatarUrl       :: Text
                        , _ownerLogin      :: Text
                        , _ownerUrl        :: Text
                        , _ownerId         :: Int
                        , _ownerGravatarId :: Maybe Text }
  deriving (Eq, Ord, Show)


-- | 'HIssue'
data HIssue
  = HIssue {  _hIssueClosedAt    :: Maybe UTCTime
            , _hIssueUpdatedAt   :: Maybe UTCTime
            , _hIssueEventsUrl   :: Text
            , _hIssueHtmlUrl     :: Maybe Text
            , _hIssueClosedBy    :: Maybe Text
            , _hIssueLabels      :: [RepoLabel]
            , _hIssueNumber      :: Int
            , _hIssueAssignee    :: Maybe Text
            , _hIssueUser        :: Text
            , _hIssueTitle       :: Text
            , _hIssuePullRequest :: Maybe Github.PullRequestReference
            , _hIssueUrl         :: String
            , _hIssueCreatedAt   :: Github.GithubDate
            , _hIssueBody        :: Maybe String
            , _hIssueState       :: String
            , _hIssueId          :: Int
            , _hIssueComments    :: Int
            , _hIssueMilestone   :: Maybe Github.Milestone}
  deriving (Eq,Ord,Show)


-- | 'HNewIssue' reflects the places where a datatype has been created 'H' for heading
-- to facilitate type checking before posting a new issue.
data HNewIssue = HNewIssue { _hNewIssueTitle     :: Text
                           , _hNewIssueBody      :: Text
                           , _hNewIssueAssignee  :: Text
                           , _hNewIssueMilestone :: Maybe Int
                           , _hNewIssueLabels    :: [RepoLabel]}
  deriving (Eq,Ord,Show)
