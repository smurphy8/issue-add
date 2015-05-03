{-# LANGUAGE OverloadedStrings #-}
module Action.Internal.Label where
import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Monoid          ((<>))
import           Data.Traversable
import           Github.Auth
import qualified Github.Issues.Labels as Github
-- | Won't work till my repo is merged


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



data RepoLabel = Bug
                 |Test
                 |Enhancement
                 |Documentation
                 |Question
                 |UserInterface
                 |Performance
                 |MonitoringLogging
                 |NetworkIssue
                 |Deployment
                 |Regression
                 |DependencyManagement
                 |CustomerRequest
                 |FieldServiceRequest
                 |NeedDiscussion
                 |NeedTestCase
                 |NeedInfo
                 |Duplicate
                 |Wontfix
                 |Ready
                 |InProgress
 deriving (Show,Eq,Ord)


toLabelStringPair :: RepoLabel -> (String,String)
toLabelStringPair Bug                       =  ("FF0000" ,"Bug"           )
toLabelStringPair Test                      =  ("FF7400" ,"Test"          )
toLabelStringPair Enhancement               =  ("00CFCF" ,"Enhancement"   )
toLabelStringPair Documentation             =  ("009191" ,"Documentation" )
toLabelStringPair Question                  =  ("C02A1A" ,"Question"      )
toLabelStringPair UserInterface             =  ("6608C5" ,"User Interface"        )
toLabelStringPair Performance               =  ("A76FDF" ,"Performance"           )
toLabelStringPair MonitoringLogging         =  ("D50090" ,"Monitoring/Logging"    )
toLabelStringPair NetworkIssue              =  ("C0A0E1" ,"Network Issue"         )
toLabelStringPair Deployment                =  ("755B00" ,"Deployment"            )
toLabelStringPair Regression                =  ("FFC600" ,"Regression"            )
toLabelStringPair DependencyManagement      =  ("FFE075" ,"Dependency Management" )
toLabelStringPair CustomerRequest           =  ("BAE5E8" ,"Customer Request"      )
toLabelStringPair FieldServiceRequest       =  ("D3EbED" ,"Field Service Request" )
toLabelStringPair NeedDiscussion            =  ("C0B51A" ,"Need Discussion" )
toLabelStringPair NeedTestCase              =  ("8c884d" ,"Need Test Case"  )
toLabelStringPair NeedInfo                  =  ("FFEE00" ,"Need Info"       )
toLabelStringPair Duplicate                 =  ("6E3D5B" ,"Duplicate"       )
toLabelStringPair Wontfix                   =  ("7F516D" ,"Wontfix"         )
toLabelStringPair Ready                     =  ("138d3c" ,"Ready"           )
toLabelStringPair InProgress                =  ("00a538" ,"In Progress"     )





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
            ,InProgress]

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
fromLabelString str                       =  Left  $ str <>
                                                    "Not one of"   <>
                                                    (unwords . fmap show $ labelList)

toLabelColorString :: RepoLabel -> String
toLabelColorString Bug                  =  "FF0000"
toLabelColorString Test                 =  "FF7400"
toLabelColorString Enhancement          =  "00CFCF"
toLabelColorString Documentation        =  "009191"
toLabelColorString Question             =  "C02A1A"
toLabelColorString UserInterface        =  "6608C5"
toLabelColorString Performance          =  "A76FDF"
toLabelColorString MonitoringLogging    =  "D50090"
toLabelColorString NetworkIssue         =  "C0A0E1"
toLabelColorString Deployment           =  "755B00"
toLabelColorString Regression           =  "FFC600"
toLabelColorString DependencyManagement =  "FFE075"
toLabelColorString CustomerRequest      =  "BAE5E8"
toLabelColorString FieldServiceRequest  =  "D3EbED"
toLabelColorString NeedDiscussion       =  "C0B51A"
toLabelColorString NeedTestCase         =  "8c884d"
toLabelColorString NeedInfo             =  "FFEE00"
toLabelColorString Duplicate            =  "6E3D5B"
toLabelColorString Wontfix              =  "7F516D"
toLabelColorString Ready                =  "138d3c"
toLabelColorString InProgress           =  "00a538"

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
fromLabelColorString ('#':str) = fromLabelColorString str
fromLabelColorString str = Left "String does not match a label color: " ++ str
