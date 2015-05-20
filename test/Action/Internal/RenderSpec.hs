{-# LANGUAGE TemplateHaskell #-}
module Action.Internal.RenderSpec (main, spec) where


import           Action.Internal.Render
import           Control.Applicative
import           Control.Lens           (folded, over, toListOf, traverse,
                                         views, (&), (.~))
import           Data.Either
import           Data.String.Here       (here)
import           Data.Text              (pack)
import           Language.Haskell.TH    (nameBase)
import           Test.Hspec
import           Test.QuickCheck


main = undefined
spec = undefined
