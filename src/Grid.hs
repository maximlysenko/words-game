module Grid
    ( Cell (Cell, Indent)
    , Grid
    , cell2Char
    , fillInBlanks
    , findWord
    , gridWithCoords
    , makeRandomGrid
    , mapOverGrid
    ) where

import Data.List (transpose)
import Data.Maybe (catMaybes, listToMaybe)
import System.Random (RandomGen, randomRs, split)

data Cell = Cell (Integer, Integer) Char
          | Indent
            deriving (Eq, Ord, Show)

type Grid a = [[a]]

cell2Char :: Cell -> Char
cell2Char (Cell _ c) = c
cell2Char Indent = '?'

coordsGrid :: Grid (Integer, Integer)
coordsGrid =
    let rows = map repeat [0..]
        cols = repeat [0..]
    in zipOverGrid rows cols

diagonilize :: Grid Cell -> Grid Cell
diagonilize = transpose . skew

fillInBlanks :: (RandomGen g) => g -> Grid Char -> Grid Char
fillInBlanks gen grid =
    let randomGrid = makeRandomGrid gen
        fill '_' randomChar = randomChar
        fill char _         = char
    in zipOverGridWith fill grid randomGrid

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
    let gridLines = getLines grid
        found = map (findWordInLine word) gridLines
    in listToMaybe (catMaybes found)

findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x:xs) (c:cs) | x == cell2Char c
    = findWordInCellLinePrefix (c : acc) xs cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
    let found = findWordInCellLinePrefix [] word line
    in case found of
        Nothing -> findWordInLine word (tail line)
        cs@(Just _) -> cs

getLines :: Grid Cell -> [[Cell]]
getLines grid = 
    let horizontal = grid
        vertical = transpose grid
        diagonal1 = diagonilize horizontal
        diagonal2 = diagonilize (map reverse horizontal)
        gridLines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in gridLines ++ (map reverse gridLines)

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords = zipOverGridWith Cell coordsGrid

makeRandomGrid :: (RandomGen g) => g -> Grid Char
makeRandomGrid gen =
    let (genA, genB) = split gen
        row = randomRs ('A', 'Z') genA
    in  row : makeRandomGrid genB

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l:ls) = l : skew (map indent ls)
    where indent line = Indent : line

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith
