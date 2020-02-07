module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Lib
import Drawings
import Game
import Models
import Block
import Ball
import Bat
import System.IO
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent.Async
import System.Environment
import System.Exit

window :: Display
window = InWindow "BallEmAll" (1000, 700) (10, 10)

background :: Color
background = white

draw :: Game -> IO Picture
draw = pure . getPicture

main :: IO ()
main = do
  savedGame <- getArgs
  case savedGame of
    [] -> playIO window background 60 initialGame draw onEvent onUpdateGame
            where
              onEvent :: Event -> Game -> IO Game
              onEvent (EventKey (SpecialKey KeyLeft) Down modifiers (x, y)) (Game blocks bat ball time settings state dbs his) = pure $ Game blocks (moveLeft bat) ball time settings state dbs his
              onEvent (EventKey (SpecialKey KeyRight) Down modifiers (x, y)) (Game blocks bat ball time settings state dbs his) = pure $ Game blocks (moveRight bat) ball time settings state dbs his
              onEvent (EventKey (SpecialKey KeyLeft) Up modifiers (x, y)) (Game blocks bat ball time settings state dbs his) = pure $ Game blocks (stopMovingLeft bat) ball time settings state dbs his
              onEvent (EventKey (SpecialKey KeyRight) Up modifiers (x, y)) (Game blocks bat ball time settings state dbs his) = pure $ Game blocks (stopMovingRight bat) ball time settings state dbs his
              onEvent (EventKey (SpecialKey KeyEsc) Down modifiers (x, y)) game = exitSuccess
              onEvent _ game = pure $ game
              onUpdateGame :: Float -> Game -> IO Game
              onUpdateGame t (Game blocks bat ball time settings GameOver dbs his) = saveGame "test.txt" (Game blocks bat ball time settings GameOver dbs his)
              onUpdateGame t game = pure $ updateGame t game
    [file] -> do
        contents <- readFile file
        let bats = read contents :: [Position]
        playIO window background 60 (Replay bats initialGame) (draw . replayGame) noEvents onUpdateReplay
          where
            noEvents :: Event -> Replay -> IO Replay
            noEvents _ = pure
            onUpdateReplay :: Float -> Replay -> IO Replay
            onUpdateReplay t = pure . updateReplay t


saveGame :: String -> Game -> IO Game
saveGame name (Game _ _ _ _ _ _ _ his) = do
  writeFile name (show (map (\ (Game _ (Bat shape (x, y) d) _ _ _ _ _ _) -> (x, y)) (reverse his)))
  return initialGame