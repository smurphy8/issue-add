{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Action.Internal.RenderSpec (main, spec) where


import           Action.Internal.Issue
import           Action.Internal.Render
import           Control.Applicative
import           Control.Lens                           (folded, over, toListOf,
                                                         traverse, views, (&),
                                                         (.~))
import           Data.Either
import           Data.String.Here                       (here)
import           Data.Text                              (pack)
import           Language.Haskell.TH                    (nameBase)
import           Test.Hspec
import           Test.QuickCheck

import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Types



eitherExample = parseOrgMode . pack $ exampleString


describeTH names = describe (unwords $ nameBase <$> names)

main = hspec spec

spec :: Spec
spec = do
   describeTH ['parseOrgMode,'renderHeading] $ do
     it "should parse a golden test and render it back consistently" $ do
        let (Right exDoc ) = eitherExample

        exDoc `shouldBe` exDoc

