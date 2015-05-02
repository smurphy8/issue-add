module Action.Internal.Label where
import           Data.Traversable

import           Github.Auth
import qualified Github.Issues.Labels as Github
-- | Won't work till my repo is merged



addLabelsToRepo authUser authPass repoUser repoName = traverse labelMaker labelList
  where
    labelMaker (c,l) = Github.createLabel auth repoUser repoName l c
    auth = GithubBasicAuth authUser authPass


labelList :: [(String, String)]
labelList = [("FF0000" ,"Bug"           )
            ,("FF7400" ,"Test"          )
            ,("00CFCF" ,"Enhancement"   )
            ,("009191" ,"Documentation" )
            ,("C02A1A" ,"Question"      )
            ,("6608C5" ,"User Interface"        )
            ,("A76FDF" ,"Performance"           )
            ,("D50090" ,"Monitoring/Logging"    )
            ,("C0A0E1" ,"Network Issue"         )
            ,("755B00" ,"Deployment"            )
            ,("FFC600" ,"Regression"            )
            ,("FFE075" ,"Dependency Management" )
            ,("BAE5E8" ,"Customer Request"      )
            ,("D3EbED" ,"Field Service Request" )
            ,("C0B51A" ,"Need Discussion" )
            ,("8c884d" ,"Need Test Case"  )
            ,("FFEE00" ,"Need Info"       )
            ,("6E3D5B" ,"Duplicate"       )
            ,("7F516D" ,"Wontfix"         )
            ,("138d3c" ,"Ready"           )
            ,("00a538" ,"In Progress"     ) ]
