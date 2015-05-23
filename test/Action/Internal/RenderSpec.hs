{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Action.Internal.RenderSpec (main, spec) where


import           Action.Internal.Issue
import           Action.Internal.Render
import           Control.Applicative
import           Control.Lens                           (folded, over, toListOf,
                                                         traverse, views, (&),
                                                         (.~))
import           Data.Either
import           Data.String.Here                       (here)
import           Data.Text                              (Text, pack)
import           Language.Haskell.TH                    (nameBase)
import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad.Fix
import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Types

-- | Heading Golden Test Code

headingExample = [here|
* TODO [#A] This is the issue title [0%] :Bug:
** ASSIGN  @smurphy8
**   Events|]


-- | SOme really dirty functions to get me what I need to make all this work

eitherExample = parseOrgMode headingExample
(h:headings) = documentHeadings doc
  where
   (Right doc)= eitherExample

-- |little sugar for the naming
describeTH names = describe (unwords $ nameBase <$> names)

-- | functions look for a fixed point to establish an adjunction
-- | Fix point on  Rendering
fixRenderFunction  = fmap (renderHeading.head.documentHeadings) .
                                          parseOrgMode
-- | Fixed point on parsing
fixHeadingFunction = fmap (head.documentHeadings) . parseOrgMode .
                     renderHeading

runFixN f n' i' = either (const False) (const True) $ loop n' i'
   where
    loop n i = do
     v <- f i
     if (v == i)
     then return i
     else if (n <= 0)
          then Left "Failed to fix"
          else loop (n-1) v




main = hspec spec

spec :: Spec
spec = do
   describeTH ['renderHeading] $ do
     it "checks a simple example for a fixed point on parsing and rendering" $ do
        True `shouldBe` runFixN fixRenderFunction 5 headingExample
        True `shouldBe`  runFixN fixHeadingFunction 5 h
