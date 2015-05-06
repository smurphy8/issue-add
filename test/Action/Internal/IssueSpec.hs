{-# LANGUAGE TemplateHaskell #-}

module Action.Internal.IssueSpec (main, spec) where

import           Action.Internal.Issue
import           Action.Internal.Label (RepoLabel (..), fromLabelColorString,
                                        fromLabelString, labelList,
                                        toLabelColorString, toLabelString)
import           Control.Applicative
import           Control.Lens          (folded, over, toListOf, traverse, views,
                                        (&), (.~))
import           Data.Either
import           Data.String.Here      (here)
import           Data.Text             (pack)
import           Language.Haskell.TH   (nameBase)
import           Test.Hspec
import           Test.QuickCheck


-- | Gen Some Data
instance Arbitrary RepoLabel where
  arbitrary = elements labelList
  shrink lbl
    |lbl > Bug = [pred lbl]
    |otherwise = []

generateHNewIssue :: Gen HNewIssue
generateHNewIssue = HNewIssue <$>
                     fmap pack arbitrary <*>
                     fmap pack arbitrary <*>
                     fmap pack arbitrary <*>
                     arbitrary <*>
                     arbitrary

instance Arbitrary HNewIssue where
  arbitrary = generateHNewIssue
  shrink hni = allShrinks
    where
     repoShrinks :: [[RepoLabel]]
     repoShrinks = views hNewIssueLabels shrink hni
     allShrinks :: [HNewIssue]
     allShrinks =  (\s -> hni & hNewIssueLabels .~ s) <$>
                   repoShrinks



main :: IO ()
main = hspec spec

-- | This spec is just here to make sure the API stays consistant when I add and remove files



lst :: [RepoLabel]
lst = labelList

spec :: Spec
spec = do
  describeTH ['toNewIssue , 'fromNewIssue] $
   it "should round trip from HNewIssue" $
     property $ \hni -> (fromNewIssue.toNewIssue $ hni) == hni
  describeTH ['toDocument, 'fromDocument] $
   it "should create a document that will settle on a form" $ do
     let      (Right firstDocument)    = parseOrgMode tst
              firstFormIssue           = fromDocument firstDocument
              reducedFormDocument      = toDocument firstFormIssue
              reducedFormIssue = fromDocument reducedFormDocument
     reducedFormIssue `shouldBe` firstFormIssue
 where
   describeTH names = describe (unwords $ nameBase <$> names)
