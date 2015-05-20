

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


-- | 'HIssue'
data HIssue
  = HIssue {  _hIssueClosedAt       :: Maybe UTCTime
            , _hIssueUpdatedAt      :: Maybe UTCTime
            , _hIssueEventsUrl      :: Text
            , _hIssueHtmlUrl        :: Maybe Text
            , _hIssueClosedBy       :: Maybe Text
            , _hIssueLabels         :: [RepoLabel]
            , _hIssueNumber         :: Int
            , _hIssueAssignee       :: Text
            , _hIssueUser           :: Text
            , _hIssueTitle          :: Text
            , _hIssuePullRequestUrl :: Text
            , _hIssueUrl            :: String
            , _hIssueCreatedAt      :: UTCTime
            , _hIssueBody           :: Text
            , _hIssueState          :: String
            , _hIssueId             :: Int
            , _hIssueComments       :: Int
            , _hIssueMilestone      :: Maybe Int}
  deriving (Eq,Ord,Show)


-- | 'HNewIssue' reflects the places where a datatype has been created 'H' for heading
-- to facilitate type checking before posting a new issue.
data HNewIssue = HNewIssue { _hNewIssueTitle     :: Text
                           , _hNewIssueBody      :: Text
                           , _hNewIssueAssignee  :: Text
                           , _hNewIssueMilestone :: Maybe Int
                           , _hNewIssueLabels    :: [RepoLabel]}
  deriving (Eq,Ord,Show)
