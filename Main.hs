{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as Map

data Color = Green
type Coord = (Int, Int)
type Board = Map.Map Coord Color
type Shape = [Coord]
data Piece = Piece Shape Coord
data Game = Game Board Piece

-- 10 x 30
line, ell, jay, square, zed, ess, tee :: Shape

line = [(0,0), (1,0), (2,0), (3,0)]
ell = [(0,1), (0,0), (1,0), (2,0)]
jay = [(2,1), (0,0), (1,0), (2,0)]
square = [(1,1), (1,0), (0,1), (0,0)]
zed = [(0,1), (1,1), (1,0), (2,0)]
ess = [(0,0), (1,1), (1,0), (2,1)]
tee = [(0,0), (1,0), (2,0), (1,1)]

aPiece :: Piece
aPiece = Piece ell (6,10)

aBoard :: Board
aBoard = Map.insert (5,3) Green $ Map.insert (5,6) Green $ Map.insert (5,5) Green Map.empty

aGame :: Game
aGame = Game aBoard aPiece

getLineStr :: Board -> Int -> String
getLineStr board y = '|' : [ if Map.member (x, y) board
                              then '*'
                              else ' ' | x <- [0..9] ] ++ "|"

getBoardStr :: Board -> String
getBoardStr board = unlines [ getLineStr board y | y <- [30,29..0]]

translate :: Piece -> [Coord]
translate (Piece s origin) = map (addCoords origin) s

addCoords :: Coord -> Coord -> Coord
addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

merge :: Board -> Piece -> Board
merge b p = foldr (\coord board -> Map.insert coord Green board) b worldPiece
  where worldPiece = translate p

conflicts :: Board -> Piece -> Bool
conflicts b p = any hit worldPiece
  where worldPiece = translate p
        hit c = Map.member c b

move :: Coord -> Piece -> Piece
move c (Piece s origin) = Piece s (addCoords origin c)

down :: Piece -> Piece
down = move (0, -1)

left :: Piece -> Piece
left = move (-1, 0)

right :: Piece -> Piece
right = move (1, 0)

rotateLeft :: Piece -> Piece
rotateLeft = rotateRight . rotateRight . rotateRight

rotateRight :: Piece -> Piece
rotateRight (Piece cs o) = Piece cs' o
  where cs' = map f cs
        f (x, y) = (y, -x)

main :: IO ()
main = putStr $ getBoardStr $ merge aBoard ((rotateRight . rotateRight) aPiece)
