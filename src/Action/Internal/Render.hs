{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

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





renderHeading :: Heading -> Text
renderHeading heading = outline
   where
     renderLevel = T.replicate starCount "*"
     renderedKeyword = views _Just unStateKeyword maybeKeyword
     renderedPriority = views _Just show maybePriority
     renderedTitle = title
     renderedStats = views _Just renderStats maybeStats
     renderedTags = ":" <> T.intercalate "::" tags <> ":"
     renderedSubHeadings = views folded renderHeading subHeadings
     (Heading{level  = (Level starCount)
             ,keyword = maybeKeyword
             ,priority = maybePriority
             ,title
             ,stats = maybeStats
             ,tags
             ,section
             ,subHeadings}) = heading
     outline = [i|
${renderLevel} ${renderedKeyword} [${renderedPriority}] ${renderedTitle} [${renderedStats}] ${renderedTags}

${renderedSubHeadings}
|]

-- | Only renders the inner part of the statistic for now
renderStats (StatsPct i) = (pack.show $ i) <> "%"
renderStats (StatsOf x y) = (pack.show $ x) <> "/" <>
                            (pack.show $ y)
