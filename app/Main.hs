module Main (main) where

import System.IO
import System.Random
import Data (grid, languages)
import Game (Game, formatGame, isCompleted, makeGame, playGame)
import Grid (fillInBlanks)

main :: IO ()
main = do
    gen <- newStdGen
    let filledGrid = fillInBlanks gen grid
        game = makeGame filledGrid languages
    hSetBuffering stdout NoBuffering
    playTurn game

playTurn :: Game -> IO ()
playTurn game = do
    putStrLn . formatGame $ game
    putStrLn "Please enter a word> "
    word <- getLine
    let newGame = playGame game word
    if isCompleted newGame then
        putStrLn "You won!"
    else
        playTurn newGame
