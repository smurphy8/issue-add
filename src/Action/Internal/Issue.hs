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
-- import           Data.Monoid                            ((<>))
import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Types
import           Data.String.Here                       (here)
import           Data.Text                              (Text, pack, unpack)
import qualified Data.Text                              as T
import qualified Data.Text.IO                           as TIO
import           Data.Time
import qualified Github.Auth                            as Github
import           Github.Issues                          (Issue (..),
                                                         NewIssue (..))
import qualified Github.Issues                          as Github
import qualified System.IO                              as IO
import           Text.Read                              (readMaybe)
{--  NewIssue Format example
      newiss = (Github.newIssue "A new issue") {
          Github.newIssueBody = Just "Issue description text goes here"
        , Github.newIssueLabels = Just []
        }

--}

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
























-- | 'HNewIssue' reflects the places where a datatype has been created 'H' for heading
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
                 (views hNewIssueBody (fmap unpack.maybeEmpty) hni)
                 (views hNewIssueAssignee (fmap unpack.maybeEmpty ) hni)
                 (view hNewIssueMilestone hni)
                 (views hNewIssueLabels  (Just . fmap toLabelString) hni )

 where
  maybeEmpty t = if T.null t
                    then Nothing
                    else Just t
fromNewIssue :: NewIssue -> HNewIssue
fromNewIssue  ni = HNewIssue
                     hnIssueTitle
                     hnIssueBody
                     hnAddIssueAssignee
                     maybeIssueMilestone
                     repoLabels

  where
    hnIssueTitle = pack.newIssueTitle $ ni
    hnIssueBody = maybe "" pack (newIssueBody  ni)
    hnAddIssueAssignee = maybe "" pack (newIssueAssignee  ni)
    maybeIssueMilestone = newIssueMilestone ni
    repoLabels :: [RepoLabel]
    repoLabels = toListOf (_Just.folded._Right)
                                 ((fmap.fmap)  fromLabelString . newIssueLabels $ ni)


-- | Document Conversion

fromDocument :: Document -> [HNewIssue]
fromDocument = rights . fmap fromHeader . documentHeadings

toDocument :: [HNewIssue] -> Document
toDocument hnis = Document "" (fmap toHeader hnis)

-- | It would be nice to make an exact round trip between
-- |headers and Issues but it can't quite work like that
-- New Issues start with a TODO
-- Issues starting with anything else are ignored
fromHeader :: Heading -> Either String HNewIssue
fromHeader hdng
  |validNewIssueHeading hdng = Right $ HNewIssue
                                                                    hnIssueTitle
                                                                    hnIssueBody
                                                                    (headingToIssueAssignee hdng)
                                                                    mileStones
                                                                    repoLabels
  | otherwise = Left $ "format Like: " ++ tstString
  where
    hnIssueTitle = title hdng
    hnIssueBody = sectionParagraph.section $ hdng
    mileStones = parseMilestone .sectionProperties . section $ hdng
    repoLabels :: [RepoLabel]
    repoLabels = rights $ fmap (fromLabelString . unpack) .
                          tags $ hdng
    parseMilestone :: Properties -> Maybe Int
    parseMilestone mp = HM.lookup "MileStone" mp >>=
                         readMaybe.unpack
    validNewIssueHeading hdng' = isLevel hdng' (Level 1)  &&
                                 maybe False (\kw -> StateKeyword "TODO" == kw)
                                 (keyword hdng')




-- |Issue Assignees must be added at the second level heading
--  with Keyword ASSIGN
headingToIssueAssignee :: Heading -> Text
headingToIssueAssignee hdng
   | not cond  = parseUser.title . head $ assignedSubs --head is checked in cond I feel this is okay
   | otherwise = ""                                    -- I could be talked out of it ...
 where
   subs = subHeadings hdng
   assignedSubs = filter assignmentHeadingFilter subs
   cond = null assignedSubs
   assignmentHeadingFilter heading'
     |isLevel heading'. Level $ 2 = maybe False (== StateKeyword "ASSIGN") . keyword $ heading'
     |otherwise = False



parseUser :: Text -> Text
parseUser t = either (const "") id runParser
 where
   runParser = parseOnly userParser t
   userParser = (char '@' *> nameParse) <|> (anyChar *> userParser)
   nameParse = do ltrs <- many' (letter <|> digit)
                  return . pack  $ '@':ltrs



-- | toHeader produces a TODO task with assignment to a user and other things
toHeader :: HNewIssue -> Heading
toHeader hni = Heading {
                   level = Level 1
                 , keyword = Just . StateKeyword $ "TODO"
                 , priority = Nothing
                 , title = hni ^. hNewIssueTitle
                 , stats = Nothing
                 , tags = views hNewIssueLabels (fmap (pack.toLabelString)) hni
                 , section = toSectionBody hni
                 , subHeadings = toAssignmentHeading hni }
          where
           toAssignmentHeading :: HNewIssue -> [Heading]
           toAssignmentHeading hni'
              | not . T.null $ assignee = [Heading { level = Level 2
                                                         , keyword = Just . StateKeyword $ "ASSIGN"
                                                         , priority = Nothing
                                                         , title = assignee
                                                         , stats = Nothing
                                                         , tags = []
                                                         , section = emptySection
                                                         , subHeadings = []  }]
              | otherwise = []
             where
               assignee = hni' ^. hNewIssueAssignee
-- | default section useful for rendering
emptySection :: Section
emptySection = Section {
                       sectionPlannings = Plns HM.empty
                     , sectionClocks = []
                     , sectionProperties = HM.empty
                    , sectionParagraph = "" }


-- | names in sections are present to make it easier to read
-- | the milestones enter the Section as Map "MileStone" Int
toSectionBody :: HNewIssue -> Section
toSectionBody hni = Section {
                       sectionPlannings = Plns HM.empty
                       , sectionClocks = []
                       , sectionProperties = issueProps
                       , sectionParagraph = paragraph }
  where
     issueProps  = views hNewIssueMilestone addToMap hni
     addToMap = maybe HM.empty (\i -> HM.insert "MileStone" (pack.show $ i) HM.empty)
     paragraph = hni ^. hNewIssueBody




-- | Example
-- | (Right rslt) = parseOnly (parseDocument ["TODO"]) tst
-- first version won't implement the issue number stuff
tst :: Text
tst = pack tstString

tstString :: String
tstString = [here|

sdfjsdf
* TODO This is the issue title :Bug:
** ASSIGN @smurphy8
:PROPERTIES:
:MileStone: 123
:END:
Here is the message body
* TODO Here is another issue :HereIsAnotherLabel:
Here is another body
* TODO Here is another issue :Clean:
I would like this to be a working thing
* TODO Generate new issue :Bug:
This is a test of issue generation
* TODO Add org-file issue tracking :Enhancement: :User Interface:
It would be nice if things like milestones
would be automatically added in


|]

-- |Network
createNewIssue
  :: ByteString
     -> ByteString -> String -> String -> NewIssue -> IO ()
createNewIssue user pass owner repo newiss= do
  let auth = Github.GithubBasicAuth user pass
  possibleIssue <- Github.createIssue auth owner repo newiss
  either (\e -> IO.hPutStrLn IO.stderr $ "Error: " ++ show e)
         (putStrLn . formatIssue)
         possibleIssue




parseIssueFromDoc :: ByteString -> ByteString -> String -> String -> FilePath -> IO ()
parseIssueFromDoc user pass owner repo fp = do
  doc <- TIO.readFile fp
  let auth = Github.GithubBasicAuth user pass
      eitherNewIssues = fmap (fmap toNewIssue.fromDocument) . parseOrgMode $ doc

  case eitherNewIssues of
    Left str -> fail str
    Right newIssues -> do
     possibleIssue <- Github.createIssue auth owner repo `traverse` newIssues
     putStrLn $ either (\e -> "Error: " ++ show e)
                     (concat . fmap formatIssue)
                     (sequence possibleIssue)
-- |Keywords
keywords :: [Text]
keywords = ["TODO","DONE","ASSIGN"]

-- |Parser
parseOrgMode :: Text -> Either String Document
parseOrgMode = parseOnly (parseDocument keywords)

-- | Printers
formatIssue :: Github.Issue -> String
formatIssue issue =
   (Github.githubOwnerLogin . Github.issueUser $ issue) ++
    " opened this issue " ++
    (show . Github.fromGithubDate . Github.issueCreatedAt $ issue) ++ "\n" ++
    Github.issueState issue ++ " with " ++
    (show . Github.issueComments $ issue) ++ " comments\n\n" ++
    Github.issueTitle issue
tst2 :: Text
tst2 = pack [here|
** TODO [#B] Polish Poetry Essay [25%] :HOMEWORK:POLISH:WRITING:
done

|]


-- | Filters and checks


-- | Respecting the NewType, check the level of the thing
isLevel :: Heading -> Data.OrgMode.Parse.Types.Level -> Bool
isLevel hdng l = (level hdng) == l

-- | Types


