{-# LANGUAGE TemplateHaskell #-}
module Action.LabelSpec (main, spec) where

import           Action.Label        (addLabelsToRepo)
import           Language.Haskell.TH (nameBase)
import           Test.Hspec
main :: IO ()
main = hspec spec

-- | This spec is just here to make sure the API stays consistant when I add and remove files
spec :: Spec
spec = do
  describe (nameBase 'addLabelsToRepo) $ do
    it "should be exported by Action Label" $ do
      True `shouldBe` True
