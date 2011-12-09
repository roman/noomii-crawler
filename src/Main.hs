{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (SomeException)

----------

import Data.Configurator.Types (AutoConfig(..))

import qualified Data.Configurator as Config

----------

import Noomii.Crawler (crawlNoomii)
import Noomii.EmailNotification (sendEmailWithStats)

-------------------------------------------------------------------------------


autoConfig :: AutoConfig
autoConfig
  = AutoConfig {
    interval = 1000
  , onError  = handler
  }
  where
    handler :: SomeException -> IO ()
    handler _ = putStrLn "Some Error on the configuration load"


main :: IO ()
main = do
    (config, _) <- Config.autoReload autoConfig ["config/app.cfg"]
    user        <- Config.require config "smtp.username"
    passwd      <- Config.require config "smtp.password"
    server      <- Config.require config "smtp.server"
    env         <- Config.require config "crawl.environment"
    crawlState  <- crawlNoomii env
    sendEmailWithStats user passwd server crawlState
    putStrLn "Done."


