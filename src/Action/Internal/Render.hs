{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{- |
Module      :  Action.Internal.Render
Description :  Render Org Mode types
Copyright   :  Plow Technologies LLC
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

Given an OrgMode Document, Render the results

-}

module Action.Internal.Render  where

import           Control.Lens
import           Data.Monoid                            ((<>))
import           Data.OrgMode.Parse.Attoparsec.Document
import           Data.OrgMode.Parse.Types
import           Data.String.Here
import           Data.Text                              (Text, pack)
import qualified Data.Text                              as T




-- | Render each part of a Heading according to standard rule
-- currently a heading with the following features would be fully rendered.
-- | ------------------
{-|
* TODO [#A] This is the issue title [0%] :Bug:
** ASSIGN  @smurphy8


|-}
-- | ------------------


renderHeading :: Heading -> Text
renderHeading heading = outline
   where
     renderLevel = T.replicate starCount "*"
     renderedKeyword = views _Just unStateKeyword maybeKeyword
     renderedPriority = renderPriority maybePriority
     renderedTitle = title
     renderedStats = renderMaybeStats maybeStats
     renderedTags = renderTags tags
     renderedSubHeadings = views folded renderHeading subHeadings
     (Heading{level  = (Level starCount)
             ,keyword = maybeKeyword
             ,priority = maybePriority
             ,title
             ,stats = maybeStats
             ,tags
             ,section
             ,subHeadings}) = heading
     outline = [i|${renderLevel} ${renderedKeyword} ${renderedPriority} ${renderedTitle} ${renderedStats} ${renderedTags}
${renderedSubHeadings}|]


-- | ['label','label'] becomes :label::label:
renderTags :: [Tag] -> Text
renderTags tags
 |null tags = ""
 |otherwise = ":" <> T.intercalate "::" tags <> ":"

-- | Render nothing if not there else render...
-- | [8%] or [3/4]

renderMaybeStats :: Maybe Stats -> Text
renderMaybeStats (Just s) = "[" <>
                            renderStats s <>
                            "]"

renderMaybeStats Nothing = ""
renderStats :: Stats -> Text
renderStats (StatsPct i) = (pack.show $ i) <> "%"
renderStats (StatsOf x y) = (pack.show $ x) <> "/" <>
                             (pack.show $ y)

-- | Render the correct priority of Org Mode
renderPriority :: Maybe Priority -> Text
renderPriority (Just p) = "[#"<> (pack.show $ p) <> "]"
renderPriority Nothing = ""


-- | Render a section... probably the most complex thing
{-|
The Section datatype
data Section
  = Section {sectionPlannings :: Plannings,
             sectionClocks :: [Data.OrgMode.Parse.Types.Clock],
             sectionProperties :: Properties,
             sectionParagraph :: Text}
   deriving (Eq,Show)

Sample target Render


* A heading
  CLOCK: [2015-05-23 Sat 00:05]--[2015-05-23 Sat 00:25] =>  0:20
  :PROPERTIES:
         :Title:     Goldberg Variations
         :Composer:  J.S. Bach
         :Artist:    Glen Gould
         :Publisher: Deutsche Grammophon
         :NDisks:    1
         :END:
section text


|-}

-- | Combine all the sub renders together
