{-# LANGUAGE OverloadedStrings #-}
module Action.Internal.Issue where

import           Data.ByteString (ByteString)
import qualified Github.Auth     as Github
import qualified Github.Issues   as Github

createNewIssue
  :: ByteString
     -> ByteString -> String -> String -> IO ()
createNewIssue user pass owner repo = do
  let auth = Github.GithubBasicAuth user pass
      newiss = (Github.newIssue "A new issue") {
          Github.newIssueBody = Just "Issue description text goes here"
        , Github.newIssueLabels = Just []
        }
  possibleIssue <- Github.createIssue auth owner repo newiss
  putStrLn $ either (\e -> "Error: " ++ show e)
                    formatIssue
                    possibleIssue
formatIssue :: Github.Issue -> String
formatIssue issue =
   (Github.githubOwnerLogin $ Github.issueUser issue) ++
    " opened this issue " ++
    (show $ Github.fromGithubDate $ Github.issueCreatedAt issue) ++ "\n" ++
    (Github.issueState issue) ++ " with " ++
    (show $ Github.issueComments issue) ++ " comments" ++ "\n\n" ++
    (Github.issueTitle issue)
