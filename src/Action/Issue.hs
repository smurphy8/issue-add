module Action.Issue ( parseIssueFromDoc
                    , createNewIssue
                    , NewIssue(..)
                     )where
import           Action.Internal.Issue (createNewIssue, parseIssueFromDoc)

import           Github.Issues         (NewIssue (..))
