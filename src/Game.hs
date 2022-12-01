module Game
    ( Game (gameGrid, gameWords)
    , formatGame
    , isCompleted
    , makeGame
    , playGame
    ) where

import qualified Data.Map as M
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Grid (Cell, Grid, cell2Char, findWord, mapOverGrid, gridWithCoords)

data Game = Game { gameGrid :: Grid Cell
                 , gameWords :: M.Map String (Maybe [Cell])
                 } deriving (Show)

isCompleted :: Game -> Bool
isCompleted game = score game == totalWords game

formatGame :: Game -> String
formatGame game = formatGameGrid game
                ++ "\n\n"
                ++ (show $ score game)
                ++ "/"
                ++ (show $ totalWords game)

formatGameGrid :: Game -> String
formatGameGrid game =
    let grid = gameGrid game
        dict = gameWords game :: M.Map String (Maybe [Cell])
        cellSet = concat . catMaybes . M.elems $ dict
        formatCell cell =
            let char = cell2Char cell
            in if cell `elem` cellSet then char else toLower char
        charGrid = mapOverGrid formatCell grid
    in unlines charGrid

makeGame :: Grid Char -> [String] -> Game
makeGame grid wordsList =
    let gwc = gridWithCoords grid
        tuplify word = (word, Nothing)
        list = map tuplify wordsList
        dict = M.fromList list
    in Game gwc dict

playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) = game
playGame game word =
    let grid = gameGrid game
        foundWord = findWord grid word
    in case foundWord of
        Nothing -> game
        Just _ ->
            let dict = gameWords game
                newDict = M.insert word foundWord dict
            in game { gameWords = newDict }

score :: Game -> Int
score = length . catMaybes . M.elems . gameWords

totalWords :: Game -> Int
totalWords = length . M.keys . gameWords
