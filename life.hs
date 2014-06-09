module Main where

import Safe (atMay) 

import Data.Maybe (catMaybes)
import Data.List (transpose)

import Control.Concurrent (threadDelay)

data Tile = On | Off deriving (Show, Eq)

type Board = [[Tile]]

main = return ()

rule :: Tile -> Int -> Tile

rule tile liveCells = case tile of
	On -> if liveCells < 2 || liveCells > 3
		then Off
		else On

	Off -> if liveCells == 3
		then On
		else Off

neighborsOf :: Board -> (Int, Int) -> [Tile]

neighborsOf board (x, y) = neighbors
	where
	neighbors = map (lookUp board) [(x', y') | x' <- [x, x+1, x-1], y' <- [y, y+1, y-1], (x', y') /= (x, y)]

testBoard :: Board
testBoard =
		[ allOff, allOff, allOff, allOff,
		fourOff ++ [On] ++ fiveOff, 
		fiveOff ++ [On] ++ fourOff, 
		threeOff ++ [On, On, On] ++ fourOff,
		allOff, allOff, allOff,
		allOff, allOff, allOff, allOff, allOff, allOff, allOff, allOff, allOff, allOff, allOff, allOff, allOff, 
		allOff ]

		where 
		allOff = take 10 (repeat Off)
		sixOff = take 6 (repeat Off)
		threeOff = take 3 (repeat Off)
		fourOff = take 4 (repeat Off)
		fiveOff = take 5 (repeat Off)
		fortyfiveOff = take 45 (repeat Off)
		fortyfourOff = take 44 (repeat Off)
		twoOff = take 2 (repeat Off)

boat :: Board
boat = [ [On, On, Off], [On, Off, On], [Off, On, Off] ]

blinker = [ [Off, On, Off], [Off, On, Off], [Off, On, Off] ]

lookUp :: Board -> (Int, Int) -> Tile 
lookUp board (x, y) = (board !! newX) !! newY
	where
	maxX = length board - 1
	maxY = length (head board) - 1
	newY = if y < 0
		then maxY
		else if y > maxY 
			then 0
			else y
	newX = if x < 0
		then maxX
		else if x > maxX
			then 0
			else x

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
		Off -> '-'

evolve :: Board -> IO ()

evolve board = do 
	prettyPrint board
	putStrLn ""
	threadDelay 100000 
	evolve newBoard
	where
	newBoard = turn board
