{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where
import           Data.Typeable
import           GHC.Generics
import           Issue
import           System.Console.GetOpt.Generics
import           System.Environment


data Options = Options {
                          repo :: String
                        , user :: String
                       }
 deriving (GHC.Generics.Generic,Typeable)

instance System.Console.GetOpt.Generics.Generic Options
instance HasDatatypeInfo Options





main = do
  options <- getArguments :: IO Options
  addLabelsToRepo (user options) (repo options)
