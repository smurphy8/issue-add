module Action.Internal.Label where
import           Control.Applicative
import           Data.Traversable
import           Github.Auth
import qualified Github.Issues.Labels as Github
-- | Won't work till my repo is merged



addLabelsToRepo authUser authPass repoUser repoName = traverse labelMaker labelList
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



toListItem :: RepoLabel -> (String,String)
toListItem Bug                       =  ("FF0000" ,"Bug"           )
toListItem Test                      =  ("FF7400" ,"Test"          )
toListItem Enhancement               =  ("00CFCF" ,"Enhancement"   )
toListItem Documentation             =  ("009191" ,"Documentation" )
toListItem Question                  =  ("C02A1A" ,"Question"      )
toListItem UserInterface             =  ("6608C5" ,"User Interface"        )
toListItem Performance               =  ("A76FDF" ,"Performance"           )
toListItem MonitoringLogging         =  ("D50090" ,"Monitoring/Logging"    )
toListItem NetworkIssue              =  ("C0A0E1" ,"Network Issue"         )
toListItem Deployment                =  ("755B00" ,"Deployment"            )
toListItem Regression                =  ("FFC600" ,"Regression"            )
toListItem DependencyManagement      =  ("FFE075" ,"Dependency Management" )
toListItem CustomerRequest           =  ("BAE5E8" ,"Customer Request"      )
toListItem FieldServiceRequest       =  ("D3EbED" ,"Field Service Request" )
toListItem NeedDiscussion            =  ("C0B51A" ,"Need Discussion" )
toListItem NeedTestCase              =  ("8c884d" ,"Need Test Case"  )
toListItem NeedInfo                  =  ("FFEE00" ,"Need Info"       )
toListItem Duplicate                 =  ("6E3D5B" ,"Duplicate"       )
toListItem Wontfix                   =  ("7F516D" ,"Wontfix"         )
toListItem Ready                     =  ("138d3c" ,"Ready"           )
toListItem InProgress                =  ("00a538" ,"In Progress"     )





labelList :: [(String, String)]
labelList = toListItem <$>  [Bug
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
