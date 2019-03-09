module Blog.Svg where

import Blog.Directory
import Data.List            (intercalate)
import Data.List.Split      (splitOn)
import Diagrams.Backend.SVG
import Diagrams.Prelude

import qualified System.Directory as IO

svgToFile :: String -> SizeSpec V2 Double -> Diagram B -> IO ()
svgToFile filename sizeSpec diagram = do
  IO.createDirectoryIfMissing True (dirname filename)
  renderSVG filename sizeSpec diagram
