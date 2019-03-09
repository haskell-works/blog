module Blog.Directory
  ( dirname
  ) where

import Data.List
import Data.List.Split

dirname :: String -> String
dirname = intercalate "/" . reverse . drop 1. reverse . splitOn "/"
