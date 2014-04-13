-- grid of cells on a board

module Main where

import Safe (atMay) 

import Data.Maybe (catMaybes)

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
	neighbors = catMaybes (map (lookUp board) [n, s, w, e, nw, ne, sw, ne])
	n = (x, y+1)
	s = (x, y-1)
	w = (x-1, y)
	e = (x+1, y)
	nw = (x+1, y-1)
	ne = (x+1, y+1)
	sw = (x-1, y-1)
	se = (x-1, y+1)

testBoard :: Board
testBoard =
		[ [On, Off]
		, [Off, On]
		]

data Maybe a = Nothing | Just a

lookUp :: Board -> (Int, Int) -> Maybe Tile 
lookUp board (x, y) = case atMay board x of
	Nothing -> Nothing
	Just column -> atMay column y 

turn :: Board -> Board

turn board = newBoard
	where 
	maxY = length board - 1
	coordinates = map makeRow [0..maxY]
	makeRow y = zip [0..maxY] (repeat y)
	liveCellsMatrix  = map (map (length.filter (== On) .neighborsOf board)) coordinates
	newBoard = zipWith (zipWith rule) board liveCellsMatrix

prettyPrint :: Board -> IO () 

prettyPrint board = mapM_ (putStrLn . map printTile) board
	where 
	printTile tile = case tile of 
		On -> 'x'
		Off -> '_'
-- in ghci - map (rule On/Off) [0..8] 
