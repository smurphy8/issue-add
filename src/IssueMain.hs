{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where
import           Action.Issue
import           Data.ByteString.Char8
import           Data.Typeable
import           GHC.Generics
import           System.Console.GetOpt.Generics


data Options = Options {
             repo     :: String
           , user     :: String
           , authUser :: String
           , pass     :: String
           , orgFile  :: String
          }
 deriving (GHC.Generics.Generic,Typeable)

instance System.Console.GetOpt.Generics.Generic Options
instance HasDatatypeInfo Options

main = do
  options <- getArguments :: IO Options
  parseIssueFromDoc (pack.authUser $ options)
                    (pack.pass $ options)
                    (user options)
                    (repo options)
                    (orgFile options)
  return ()
