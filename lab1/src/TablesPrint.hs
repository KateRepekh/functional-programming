module TablesPrint where

import Text.PrettyPrint.Boxes
import Data.List

pad width x = x ++ replicate k ' '
  where k = width - length x

fmtColumn :: [Prelude.String] -> Box
fmtColumn items = hsep // vcat left (intersperse hsep (map (text.pad width) items)) // hsep
  where width = maximum $ map length items
        hsep = text ( replicate width '-' )

getTableBox :: [[Prelude.String]] -> Box
getTableBox rows = vsep Text.PrettyPrint.Boxes.<> hcat top (intersperse vsep (map fmtColumn columns)) Text.PrettyPrint.Boxes.<> vsep
  where
    columns = transpose rows
    nrows = length rows
    vsep =  vcat left $ map char ("+" ++ (concat $ replicate nrows "|+"))

printTable :: [[Prelude.String]] -> IO()
printTable table = printBox (getTableBox table)