

{- |
Module      :  Action.Internal.Label.Types
Description :  Label Types
Copyright   :  Plow Technologies LLC
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

Lots of other parts of this package depend on the Label stuff here
'RepoLabel' is the fixed type glue that holds this whole thing together.

It is set up as a fixed parameter because I didn't want to have to do dynamic checking to make sure a label was valid every time that I wanted to parse anything.

It is a fairly comprehensive list of labels and includes all the defaultones.
-}

module Action.Internal.Label.Types (RepoLabel(..)) where

-- | All the agreed apon labels for a github repo
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
                 |Clean
 deriving (Show,Eq,Ord,Enum)
