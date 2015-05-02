{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where
import           Action.Issue
import           Data.ByteString.Char8
import           Data.Typeable
import           GHC.Generics
import           System.Console.GetOpt.Generics
import           System.Environment


data Options = Options {
             repo     :: String
           , user     :: String
           , authUser :: String
           , pass     :: String
          }
 deriving (GHC.Generics.Generic,Typeable)

instance System.Console.GetOpt.Generics.Generic Options
instance HasDatatypeInfo Options
main = do
  options <- getArguments :: IO Options
  return ()
