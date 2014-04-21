-- grid of cells on a board

module Main where

import Safe (atMay) 

import Data.Maybe (catMaybes)
import Data.List (transpose)

import Control.Concurrent (threadDelay)

data Tile = On | Off deriving (Show, Eq)

type Board = [[Tile]]

main = return ()

rule :: Tile -> Int -> Tile
-- Tile -> (Int -> Tile)
-- :: means 'has type'

rule tile liveCells = case tile of
	On -> if liveCells < 2 || liveCells > 3
		then Off
		else On

	Off -> if liveCells == 3
		then On
		else Off
-- || means 'or'

neighborsOf :: Board -> (Int, Int) -> [Tile]

neighborsOf board (x, y) = neighbors
	where
	neighbors = catMaybes (map (lookUp board) [(x', y') | x' <- [x, x+1, x-1], y' <- [y, y+1, y-1], (x', y') /= (x, y)])

testBoard :: Board
testBoard =
		[ allOff, allOff, allOff, allOff,
		fourOff ++ [On] ++ fiveOff, 
		fiveOff ++ [On] ++ fourOff, 
		threeOff ++ [On, On, On] ++ fourOff,
		allOff, allOff, allOff
		]
--	-BOAT TESTS- 
		where 
		allOff = take 10 (repeat Off)
		sixOff = take 6 (repeat Off)
		threeOff = take 3 (repeat Off)
		fourOff = take 4 (repeat Off)
		fiveOff = take 5 (repeat Off)

-- This board should be a still life.
boat :: Board
boat = [ [On, On, Off], [On, Off, On], [Off, On, Off] ]

-- This is a small periodic board.
blinker = [ [Off, On, Off], [Off, On, Off], [Off, On, Off] ]

lookUp :: Board -> (Int, Int) -> Maybe Tile 
lookUp board (x, y) = case atMay board x of
	Nothing -> Nothing
	Just column -> atMay column y 

turn :: Board -> Board

turn board = newBoard
	where 
	countNeighbors = length . filter (== On) . neighborsOf board
	liveCellsMatrix  = map (map countNeighbors) (coordinates board)
	newBoard = zipWith (zipWith rule) board liveCellsMatrix

coordinates :: Board -> [[(Int, Int)]]
coordinates board = transpose (map makeRow [0..maxY])
	where
	makeRow y = zip [0..maxY] (repeat y)
	maxY = length board - 1

prettyPrint :: Board -> IO () 

prettyPrint board = mapM_ (putStrLn . map printTile) board
	where 
	printTile tile = case tile of 
		On -> 'x'
		Off -> '_'
-- in ghci - map (rule On/Off) [0..8] 

evolve :: Board -> IO ()

evolve board = do 
	prettyPrint board
	putStrLn ""
	threadDelay 1000000 
	evolve newBoard
	where
	newBoard = turn board
