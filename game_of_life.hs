import Control.Monad
import Control.Concurrent
import Control.Monad.State
import Data.List
import Data.List.Index

data Cell = Alive | Dead deriving (Eq, Show)

data Board = Board [[Cell]]

showBoardRow :: [Cell] -> String  
showBoardRow row = intercalate " " (map 
                    (\x -> if x == Alive then "#" else "-") row)

instance Show Board where
  show (Board b) = (intercalate "\n" (map showBoardRow b)) ++ "\n"

board = Board [[Dead, Dead, Dead, Dead, Dead],
              [Dead, Dead, Dead, Dead, Dead],
              [Dead, Alive, Alive, Alive, Dead],
              [Dead, Dead, Dead, Dead, Dead],
              [Dead, Dead, Dead, Dead, Dead]]

-- Any live cell with fewer than two live neighbors dies, as if by under population.
-- Any live cell with two or three live neighbors lives on to the next generation.
-- Any live cell with more than three live neighbors dies, as if by overpopulation.
-- Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.

getLiveNeighbors :: Board -> (Int, Int) -> Int
getLiveNeighbors (Board []) _ = 0
getLiveNeighbors (Board b) (row, col) =
  let rows = ifilter (\i a -> (abs (i - row)) <= 1) (indexed b)
  in let neighbors = concat [(ifilter (\i a -> ((j, i) /= (row, col)) && (abs (i - col)) <= 1) r) | (j, r) <- rows]
  in length (filter (\neighbor -> neighbor == Alive) neighbors)

determineCell :: Cell -> Int -> Cell
determineCell Alive neighbors
  | neighbors == 2 || neighbors == 3 = Alive
  | otherwise = Dead
determineCell Dead neighbors
  | neighbors == 3 = Alive
  | otherwise = Dead
  
developBoard :: Board -> Board
developBoard (Board b) =
  Board (imap (\i row -> (
    imap (\j cell -> determineCell cell (getLiveNeighbors (Board b) (i, j))) row
  )) b)

runRound :: StateT Board IO ()
runRound = forever $ do
  board <- get
  liftIO $ putStrLn (show board)
  put (developBoard board)
  liftIO $ threadDelay (1000 * 1000)

main :: IO ()
main = do
  (_, newBoard) <- runStateT runRound board
  putStrLn (show newBoard)
