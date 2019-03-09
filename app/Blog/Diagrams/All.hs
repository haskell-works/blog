module Blog.Diagrams.All where

import Blog.Diagrams.Json.StateRailroad

genFiles :: IO ()
genFiles = Blog.Diagrams.Json.StateRailroad.genFiles
