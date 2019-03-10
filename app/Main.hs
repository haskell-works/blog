{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Blog.Diagrams.All as ALL

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

main :: IO ()
main = do
  ALL.genFiles
