{-# LANGUAGE TemplateHaskell #-}
module Action.Internal.LabelSpec (main, spec) where

import           Action.Internal.Label (RepoLabel, fromLabelColorString,
                                        fromLabelString, labelList,
                                        toLabelColorString, toLabelString)
import           Control.Applicative
import           Data.Either
import           Language.Haskell.TH   (nameBase)
import           Test.Hspec

main :: IO ()
main = hspec spec

-- | This spec is just here to make sure the API stays consistant when I add and remove files



lst :: [RepoLabel]
lst = labelList

spec :: Spec
spec = do
  describeTH ['fromLabelColorString , 'toLabelColorString] $
    it "should be exported by Action Label and return" $
      shouldBe  lst
               (rights . fmap (fromLabelColorString.toLabelColorString) $ lst)
  describeTH ['fromLabelString, 'toLabelString] $
    it "should be exported by Action Label and return" $
      shouldBe  lst
               (rights . fmap (fromLabelString.toLabelString) $ lst)
 where
   describeTH names = describe (unwords $ nameBase <$> names)
