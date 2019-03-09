module Blog.Diagrams.All where

import Blog.Diagrams.Json.StateRailroad
import Blog.Diagrams.Json.StateTransition

genFiles :: IO ()
genFiles = do
  Blog.Diagrams.Json.StateRailroad.genFiles
  Blog.Diagrams.Json.StateTransition.genFiles
