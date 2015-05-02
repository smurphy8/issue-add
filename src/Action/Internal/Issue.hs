{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Action.Internal.Issue where

import           Data.Attoparsec.Text
import           Data.ByteString                        (ByteString)
import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Types
import           Data.String.Here
import qualified Github.Auth                            as Github
import           Github.Issues                          (NewIssue (..))
import qualified Github.Issues                          as Github

import           Data.Text                              (pack)


{--  NewIssue Format example
      newiss = (Github.newIssue "A new issue") {
          Github.newIssueBody = Just "Issue description text goes here"
        , Github.newIssueLabels = Just []
        }

--}



-- |Network
createNewIssue
  :: ByteString
     -> ByteString -> String -> String -> NewIssue -> IO ()
createNewIssue user pass owner repo newiss= do
  let auth = Github.GithubBasicAuth user pass
  possibleIssue <- Github.createIssue auth owner repo newiss
  putStrLn $ either (\e -> "Error: " ++ show e)
                    formatIssue
                    possibleIssue




-- |Keywords
keywords = ["TODO","DONE"]

-- |Parser
parseOrgMode = parseOnly (parseDocument keywords)

-- | Printers
formatIssue :: Github.Issue -> String
formatIssue issue =
   (Github.githubOwnerLogin $ Github.issueUser issue) ++
    " opened this issue " ++
    (show $ Github.fromGithubDate $ Github.issueCreatedAt issue) ++ "\n" ++
    (Github.issueState issue) ++ " with " ++
    (show $ Github.issueComments issue) ++ " comments" ++ "\n\n" ++
    (Github.issueTitle issue)

tst2 = pack [here|
** TODO [#B] Polish Poetry Essay [25%] :HOMEWORK:POLISH:WRITING:
done

|]

-- | (Right rslt) = parseOnly (parseDocument ["TODO"]) tst
tst = pack [here|

* TODO This is the issue title :HereIsALabel:
:PROPERTIES:
:ISSUE: 123
:END:
Here is the message body
* Here is another issue [11] :HereIsAnotherLabel:
Here is another body

|]
