{-# LANGUAGE OverloadedStrings #-}
module Action.Internal.Label (RepoLabel (..), addLabelsToRepo,
                              fromLabelColorString, fromLabelString,
                              toLabelColorString,labelStringPairList,
                              toLabelString,labelList,toLabelStringPair) where
import           Control.Applicative
import           Control.Arrow               ((***))
import           Data.ByteString             (ByteString)
import           Data.Monoid                 ((<>))
import           Data.Traversable
import           Github.Auth
import qualified Github.Issues.Labels        as Github


-- Local
import           Action.Internal.Label.Types (RepoLabel (..))


-- | Call to the network to add all Labels

addLabelsToRepo
  :: ByteString
     -> ByteString
     -> String
     -> String
     -> IO [Either Github.Error Github.IssueLabel]
addLabelsToRepo authUser authPass repoUser repoName = traverse labelMaker labelStringPairList
  where
    labelMaker (c,l) = Github.createLabel auth repoUser repoName l c
    auth = GithubBasicAuth authUser authPass

-- | generate the Labels with their appropriate colors in string form
-- to be inserted in the repo
toLabelStringPair :: RepoLabel -> (String,String)
toLabelStringPair l = toLabelColorString *** toLabelString  $ (l,l)


-- | generate pairs for every label
labelStringPairList :: [(String, String)]
labelStringPairList = toLabelStringPair <$>  labelList

labelList :: [RepoLabel]
labelList = [Bug
            ,Test
            ,Enhancement
            ,Documentation
            ,Question
            ,UserInterface
            ,Performance
            ,MonitoringLogging
            ,NetworkIssue
            ,Deployment
            ,Regression
            ,DependencyManagement
            ,CustomerRequest
            ,FieldServiceRequest
            ,NeedDiscussion
            ,NeedTestCase
            ,NeedInfo
            ,Duplicate
            ,Wontfix
            ,Ready
            ,InProgress
            ,Clean]

-- | instead of show, a specific label converter is used
-- | that way it can be controlled w/o all the ReadS and ShowS
-- mechanics

toLabelString :: RepoLabel -> String
toLabelString Bug                       =  "Bug"
toLabelString Test                      =  "Test"
toLabelString Enhancement               =  "Enhancement"
toLabelString Documentation             =  "Documentation"
toLabelString Question                  =  "Question"
toLabelString UserInterface             =  "User Interface"
toLabelString Performance               =  "Performance"
toLabelString MonitoringLogging         =  "Monitoring/Logging"
toLabelString NetworkIssue              =  "Network Issue"
toLabelString Deployment                =  "Deployment"
toLabelString Regression                =  "Regression"
toLabelString DependencyManagement      =  "Dependency Management"
toLabelString CustomerRequest           =  "Customer Request"
toLabelString FieldServiceRequest       =  "Field Service Request"
toLabelString NeedDiscussion            =  "Need Discussion"
toLabelString NeedTestCase              =  "Need Test Case"
toLabelString NeedInfo                  =  "Need Info"
toLabelString Duplicate                 =  "Duplicate"
toLabelString Wontfix                   =  "Wontfix"
toLabelString Ready                     =  "Ready"
toLabelString InProgress                =  "In Progress"
toLabelString Clean                     =  "Clean"


-- | inverse function, notice the either strings are strings afterall
fromLabelString :: String -> Either String RepoLabel
fromLabelString "Bug"                     =  Right Bug
fromLabelString "Test"                    =  Right Test
fromLabelString "Enhancement"             =  Right Enhancement
fromLabelString "Documentation"           =  Right Documentation
fromLabelString "Question"                =  Right Question
fromLabelString "User Interface"          =  Right UserInterface
fromLabelString "Performance"             =  Right Performance
fromLabelString "Monitoring/Logging"      =  Right MonitoringLogging
fromLabelString "Network Issue"           =  Right NetworkIssue
fromLabelString "Deployment"              =  Right Deployment
fromLabelString "Regression"              =  Right Regression
fromLabelString "Dependency Management"   =  Right DependencyManagement
fromLabelString "Customer Request"        =  Right CustomerRequest
fromLabelString "Field Service Request"   =  Right FieldServiceRequest
fromLabelString "Need Discussion"         =  Right NeedDiscussion
fromLabelString "Need Test Case"          =  Right NeedTestCase
fromLabelString "Need Info"               =  Right NeedInfo
fromLabelString "Duplicate"               =  Right Duplicate
fromLabelString "Wontfix"                 =  Right Wontfix
fromLabelString "Ready"                   =  Right Ready
fromLabelString "In Progress"             =  Right InProgress
fromLabelString "Clean"                   =  Right Clean
fromLabelString str                       =  Left  $ str <>
                                                    "Not one of"   <>
                                                    (unwords . fmap toLabelString $ labelList)

-- |Each Label has a color
-- the weird drop hash thing is just so I can use color modes in emacs easily
toLabelColorString :: RepoLabel -> String
toLabelColorString Bug                  = dropHash "#FF0000"
toLabelColorString Test                 = dropHash "#FF7400"
toLabelColorString Enhancement          = dropHash "#00CFCF"
toLabelColorString Documentation        = dropHash "#009191"
toLabelColorString Question             = dropHash "#C02A1A"
toLabelColorString UserInterface        = dropHash "#6608C5"
toLabelColorString Performance          = dropHash "#A76FDF"
toLabelColorString MonitoringLogging    = dropHash "#D50090"
toLabelColorString NetworkIssue         = dropHash "#C0A0E1"
toLabelColorString CustomerRequest      = dropHash "#BAE5E8"
toLabelColorString FieldServiceRequest  = dropHash "#D3EbED"
toLabelColorString Deployment           = dropHash "#755B00"
toLabelColorString Regression           = dropHash "#FFC600"
toLabelColorString DependencyManagement = dropHash "#FFE075"
toLabelColorString NeedDiscussion       = dropHash "#C0B51A"
toLabelColorString NeedTestCase         = dropHash "#8c884d"
toLabelColorString NeedInfo             = dropHash "#FFEE00"
toLabelColorString Duplicate            = dropHash "#6E3D5B"
toLabelColorString Wontfix              = dropHash "#7F516D"
toLabelColorString Ready                = dropHash "#138d3c"
toLabelColorString InProgress           = dropHash "#00a538"
toLabelColorString Clean                = dropHash "#3333CC"
dropHash :: String -> String
dropHash ('#':str) = str
dropHash str = str



-- |These colors have labels
fromLabelColorString :: String -> Either String RepoLabel
fromLabelColorString "FF0000" = Right Bug
fromLabelColorString "FF7400" = Right Test
fromLabelColorString "00CFCF" = Right Enhancement
fromLabelColorString "009191" = Right Documentation
fromLabelColorString "C02A1A" = Right Question
fromLabelColorString "6608C5" = Right UserInterface
fromLabelColorString "A76FDF" = Right Performance
fromLabelColorString "D50090" = Right MonitoringLogging
fromLabelColorString "C0A0E1" = Right NetworkIssue
fromLabelColorString "755B00" = Right Deployment
fromLabelColorString "FFC600" = Right Regression
fromLabelColorString "FFE075" = Right DependencyManagement
fromLabelColorString "BAE5E8" = Right CustomerRequest
fromLabelColorString "D3EbED" = Right FieldServiceRequest
fromLabelColorString "C0B51A" = Right NeedDiscussion
fromLabelColorString "8c884d" = Right NeedTestCase
fromLabelColorString "FFEE00" = Right NeedInfo
fromLabelColorString "6E3D5B" = Right Duplicate
fromLabelColorString "7F516D" = Right Wontfix
fromLabelColorString "138d3c" = Right Ready
fromLabelColorString "00a538" = Right InProgress
fromLabelColorString "3333CC" = Right Clean
fromLabelColorString str = Left $ "String does not match a label color: " ++ str
