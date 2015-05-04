{-# LANGUAGE TemplateHaskell #-}
module Action.Internal.LabelSpec (main, spec) where

import           Action.Internal.Label (RepoLabel (..), addLabelsToRepo,
                                        fromLabelColorString, fromLabelString,
                                        toLabelColorString, toLabelString)
import           Language.Haskell.TH   (nameBase)
import           Test.Hspec
main :: IO ()
main = hspec spec

-- | This spec is just here to make sure the API stays consistant when I add and remove files

spec :: Spec
spec = do
  describe (nameBase 'addLabelsToRepo) $ do
    it "should be exported by Action Label" $ do
      True `shouldBe` True
