module Main (main, Board) where

import Control.Monad
import Control.Concurrent
import Control.Monad.State
import Data.List

data Board = Board [[Bool]]

showBoardRow :: [Bool] -> String  
showBoardRow row = intercalate " " (map 
                    (\x -> if x then "#" else " ") row)

instance Show Board where
  show (Board b) = (intercalate "\n" (map showBoardRow b)) ++ "\n"

board = Board [[True, False], [False, True]]

invertRow :: [Bool] -> [Bool]
invertRow row = map (\a -> not a) row

developBoard :: Board -> Board
developBoard (Board b) = Board (map invertRow b)

runRound :: StateT Board IO ()
runRound = forever $ do
  board <- get
  put (developBoard board)
  newBoard <- get
  liftIO $ putStrLn (show newBoard)
  liftIO $ threadDelay (1000 * 1000)

main :: IO ()
main = do
  (_, newBoard) <- runStateT runRound board
  putStrLn (show newBoard)

