{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Action.Internal.Issue where
import           Action.Internal.Label
import           Control.Applicative                    ((*>), (<|>))
import           Control.Lens
import           Data.Attoparsec.Text
import           Data.ByteString                        (ByteString)
import           Data.Either                            (rights)
import qualified Data.HashMap.Strict                    as HM
import           Data.Monoid                            ((<>))
import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Types
import           Data.String.Here
import           Data.Text                              (Text, pack, unpack)
import qualified Github.Auth                            as Github
import           Github.Issues                          (NewIssue (..))
import qualified Github.Issues                          as Github
import           Text.Read                              (readMaybe)

{--  NewIssue Format example
      newiss = (Github.newIssue "A new issue") {
          Github.newIssueBody = Just "Issue description text goes here"
        , Github.newIssueLabels = Just []
        }

--}

-- | DataTypes


-- | 'HNewIssue' reflects the places where a datatype has been created
-- to facilitate type checking before posting a new issue.

data HNewIssue = HNewIssue { _hNewIssueTitle     :: Text
                           , _hNewIssueBody      :: Text
                           , _hNewIssueAssignee  :: Text
                           , _hNewIssueMilestone :: Maybe Int
                           , _hNewIssueLabels    :: [RepoLabel]}
  deriving (Eq,Ord,Show)

makeLenses ''HNewIssue

toNewIssue :: HNewIssue -> NewIssue
toNewIssue hni = NewIssue
                 (views hNewIssueTitle unpack hni)
                 (views hNewIssueBody (Just . unpack) hni)
                 (views hNewIssueAssignee (Just . unpack) hni)
                 (view hNewIssueMilestone hni)
                 (views hNewIssueLabels  (Just . fmap toLabelString) hni )


fromNewIssue :: NewIssue -> HNewIssue
fromNewIssue  ni = HNewIssue
                     issueTitle
                     issueBody
                     issueAssignee
                     maybeIssueMilestone
                     repoLabels

  where
    issueTitle = pack.newIssueTitle $ ni
    issueBody = maybe "" pack (newIssueBody  ni)
    issueAssignee = maybe "" pack (newIssueAssignee  ni)
    maybeIssueMilestone = newIssueMilestone ni
    repoLabels :: [RepoLabel]
    repoLabels = toListOf (_Just.folded._Right)
                                 ((fmap.fmap)  fromLabelString . newIssueLabels $ ni)

-- | There are a few big missing pieces
-- currently, the keyword portion is bound to Nothing
toHeader :: HNewIssue -> Heading
toHeader hni = Heading
                 (Level 1)
                 Nothing
                 Nothing
                 (hni ^. hNewIssueTitle)
                 Nothing
                 (views hNewIssueLabels (fmap (pack.toLabelString)) hni)
                 (sectionBody hni)
                 []



sectionBody hni = Section
                    (Plns HM.empty)
                    []
                    issueProps
                    paragraph
  where
     issueProps  = (views hNewIssueMilestone addToMap hni )
     addToMap maybeInt = maybe HM.empty (\i -> HM.insert "IssueNumber" (pack.show $ i) HM.empty) maybeInt
     paragraph = (hni ^. hNewIssueAssignee) <> (hni ^. hNewIssueBody)


fromHeader :: Heading -> Either String HNewIssue
fromHeader hdng
  |(level hdng == Level 1) = Right $ HNewIssue
                                           issueTitle
                                           issueBody
                                           issueAssignee
                                           mileStones
                                           repoLabels
  | otherwise = Left $ "format Like: " ++ tstString
  where
    issueTitle = title hdng
    issueBody = sectionParagraph.section $ hdng
    issueAssignee = parseUser.sectionParagraph.section $ hdng
    mileStones = parseMilestone .sectionProperties . section $ hdng
    repoLabels :: [RepoLabel]
    repoLabels = rights $ fmap (fromLabelString . unpack) .
                          tags $ hdng
    parseMilestone :: Properties -> Maybe Int
    parseMilestone mp = (HM.lookup "IssueNumber" mp) >>=
                         readMaybe.unpack


parseUser :: Text -> Text
parseUser t = either (const "") id runParser
 where
   runParser = parseOnly userParser t
   userParser = (char '@' *> nameParse) <|> (anyChar *> userParser)
   nameParse = do ltrs <- many' letter
                  return . pack  $ '@':ltrs
-- | Example
-- | (Right rslt) = parseOnly (parseDocument ["TODO"]) tst
-- first version won't implement the issue number stuff
tst :: Text
tst = pack tstString

tstString :: String
tstString = [here|
* TODO This is the issue title :HereIsALabel:
:PROPERTIES:
:IssueNumber: 123
:END:
Here is the message body
* Here is another issue :HereIsAnotherLabel:
Here is another body

|]

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
keywords :: [Text]
keywords = ["TODO","DONE"]

-- |Parser
parseOrgMode :: Text -> Either String Document
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
tst2 :: Text
tst2 = pack [here|
** TODO [#B] Polish Poetry Essay [25%] :HOMEWORK:POLISH:WRITING:
done

|]

-- | Types


