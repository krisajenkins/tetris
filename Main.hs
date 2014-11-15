{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as Map

data Color = Green
data Coord = Coord (Int, Int) deriving (Eq, Ord)
type Board = Map.Map Coord Color
type Shape = [Coord]
data Piece = Piece Shape Coord
data Game = Game Board Piece

-- 10 x 30
line, ell, jay, square, zed, ess, tee :: Shape

line   = [Coord (0,0), Coord (1,0), Coord (2,0), Coord (3,0)]
ell    = [Coord (0,1), Coord (0,0), Coord (1,0), Coord (2,0)]
jay    = [Coord (2,1), Coord (0,0), Coord (1,0), Coord (2,0)]
square = [Coord (1,1), Coord (1,0), Coord (0,1), Coord (0,0)]
zed    = [Coord (0,1), Coord (1,1), Coord (1,0), Coord (2,0)]
ess    = [Coord (0,0), Coord (1,1), Coord (1,0), Coord (2,1)]
tee    = [Coord (0,0), Coord (1,0), Coord (2,0), Coord (1,1)]

aPiece :: Piece
aPiece = Piece ell (Coord (6,10))

aBoard :: Board
aBoard = foldr f Map.empty [Coord (5,3), Coord (5,6), Coord (5,5)]
  where f c = Map.insert c Green

aGame :: Game
aGame = Game aBoard aPiece

getLineStr :: Board -> Int -> String
getLineStr board y = "|" ++ fmap f [0..9] ++ "|"
  where f x = case Map.lookup (Coord (x, y)) board of
                Just Green -> 'G'
                Nothing -> ' '

getBoardStr :: Board -> String
getBoardStr board = unlines $ fmap (getLineStr board) [30,29..0]

getGameStr :: Game -> String
getGameStr (Game b p) = getBoardStr $ merge b p

translate :: Piece -> [Coord]
translate (Piece s origin) = fmap (addCoords origin) s

addCoords :: Coord -> Coord -> Coord
addCoords (Coord (x1, y1)) (Coord (x2, y2)) = Coord (x1 + x2, y1 + y2)

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
down = move (Coord (0, -1))

left :: Piece -> Piece
left = move (Coord (-1, 0))

right :: Piece -> Piece
right = move (Coord (1, 0))

class Rotateable a where
  rotateRight :: a -> a
  rotateLeft :: a -> a
  rotateLeft = rotateRight . rotateRight . rotateRight

instance Rotateable Coord where
  rotateRight (Coord (x, y)) = Coord (y, -x)

instance Rotateable Piece where
  rotateRight (Piece cs o) = Piece cs' o
    where cs' = fmap rotateRight cs

command :: String -> Piece -> Piece
command "s" = down
command "a" = left
command "d" = right
command "q" = rotateLeft
command "e" = rotateRight
command _ = id

step :: (Piece -> Piece) -> Game -> Game
step f (Game b p) = Game b (f p)

runLoop :: Game -> IO Game
runLoop game = do
  putStr $ getGameStr game
  char <- getLine
  runLoop $ step (command char) game

main :: IO Game
main = runLoop (Game aBoard aPiece)
