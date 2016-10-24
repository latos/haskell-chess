{-# LANGUAGE ScopedTypeVariables #-}

module Pieces where

import Control.Monad
import Data.Array
import Data.Char
import Data.List
import System.Console.Haskeline
import Debug.Trace
-- import Data.Foldable as F
-- import Data.List.Split

dim = 8

diags = [(-1,-1), (-1,1), (1,-1), (1,1)]
orths = [(0,-1), (0,1), (-1,0), (1,0)]
alldirs = diags ++ orths
knightdirs = [(1,2),(2,1),(-1,2),(2,-1),(1,-2),(-2,1),(-1,-2),(-2,-1)]

data Piece = King | Queen | Rook | Bishop | Knight | Pawn
  deriving Eq

pieceValue :: Piece -> Double
pieceValue King = 200.0
pieceValue Queen = 9.0
pieceValue Rook = 5.0
pieceValue Bishop = 3.25
pieceValue Knight = 3.1
pieceValue Pawn = 1


data Player = White | Black
  deriving (Show, Eq)

playerDir White =  1
playerDir Black = -1
opponent White = Black
opponent Black = White

data SqState = Occupied Player Piece | Empty

blacken = Occupied Black
whiten  = Occupied White

instance Show Piece where
  show King   = "\x2654"
  show Queen  = "\x2655"
  show Rook   = "\x2656"
  show Bishop = "\x2657"
  show Knight = "\x2658"
  show Pawn   = "\x2659"

instance Show SqState where
  -- Actually this is inverted, but it looks right on a black terminal
  show (Occupied Black piece) = show piece
  show (Occupied White piece) = [toEnum $ fromEnum (head (show piece)) + 6]
  show Empty         = "."


type Board = Array Location SqState

data Location = L Int Int
type Direction = (Int, Int)

instance Eq Location where
  a == b = (boardIndex a) == (boardIndex b)
instance Ord Location where
  a `compare` b = (boardIndex a) `compare` (boardIndex b)
instance Ix Location where
  range (a, b) = map index2loc $ range (boardIndex a, boardIndex b)
  index (a, b) x = index (boardIndex a, boardIndex b) (boardIndex x)
  inRange (a, b) x = inRange (boardIndex a, boardIndex b) (boardIndex x)

instance Show Location where
  show (L x y) = [toEnum (97 + x), toEnum (49 + y)]

readLoc :: String -> Location
readLoc [x,y] = L (bound (fromEnum x - 97)) (bound (fromEnum y - 49))


type Diff = [(Location, SqState)]

ladd (dx, dy) (L x y) = L (x + dx) (y + dy)

ray :: Int -> (Int, Int) -> Location -> [Location]
ray maxd (dx, dy) (L x y) = filter inBounds raw
  where
    raw = map (\i -> L (x + i*dx) (y + i*dy)) [1..maxd]

inBounds (L x y) = inBoundsCoord x && inBoundsCoord y

index2loc i = L (i `mod` dim) (i `div` dim)
boardIndex (L x y) = dim * y + x
isOccupied board loc = case board ! loc of
  Empty -> False
  _     -> True

isOccupiedBy player board loc = ownsSquare player (board ! loc)

ownsSquare player sq = case sq of
  Occupied p _ -> player == p
  _            -> False


allLocs = range (a1, h8)

possiblePlayerMoves :: Player -> Board -> [Diff]
possiblePlayerMoves player board = filter (isOccupiedBy player board) allLocs >>= possibleMoves board

  

possibleMoves :: Board -> Location -> [Diff]
possibleMoves board start
  | piece == Pawn = pawnMoves board start
  | otherwise     = map rayMove (possibleRayMoveLocs piece False player board start)
  where 
    Occupied player piece = board ! start
    rayMove end = [(end, Occupied player piece), (start, Empty)]


-- for moving, attacking, or defending
possibleTargets :: Board -> Location -> [Location]
possibleTargets board start
  | piece == Pawn = pawnMoveLocs True board start
  | otherwise     = possibleRayMoveLocs piece True player board start
  where Occupied player piece = board ! start

possibleRayMoveLocs :: Piece -> Bool -> Player -> Board -> Location -> [Location]
possibleRayMoveLocs King = rayDests 1 alldirs
possibleRayMoveLocs Queen = rayDests dim alldirs
possibleRayMoveLocs Rook = rayDests dim orths
possibleRayMoveLocs Bishop = rayDests dim diags
possibleRayMoveLocs Knight = rayDests 1 knightdirs

rayDests :: Int -> [Direction] -> Bool -> Player -> Board -> Location -> [Location]
rayDests maxd dirs includeSelf player board start = map rayPrefix dirs >>= id
  where
    rayPrefix dir = legalPrefix (ray maxd dir start)
    legalPrefix [] = []
    legalPrefix (x:xs) = case board ! x of
      Empty -> x : legalPrefix xs
      Occupied p _ -> if includeSelf || p == opponent player then [x] else []


pawnMoves :: Board -> Location -> [Diff]
pawnMoves board start = map withRemovedStart allDests
  -- TODO: en passant
  where
    allDests :: [(Location, SqState)]
    allDests = (pawnMoveLocs False board start) >>= expandDests

    expandDests :: Location -> [(Location, SqState)]
    expandDests loc@(L _ row) = map (\piece -> (loc, Occupied player piece)) pieces
      where pieces
              | isLastRow player row = [Queen, Knight] -- useful upgrades
              | otherwise            = [Pawn]

    isLastRow White row = row == 7
    isLastRow Black row = row == 0

    withRemovedStart end = [end, (start, Empty)]

    Occupied player piece = board ! start

pawnMoveLocs :: Bool -> Board -> Location -> [Location]
pawnMoveLocs includeSelf board start = (pawnWalks ++ pawnTakes)
  where

    pawnWalks = let L _ row = start in pawnWalkPrefix (ray (walkLen player row) (0, dir) start)
    pawnTakes = filter canTakeAt [(ladd (-1, dir) start), (ladd (1, dir) start)]

    canTakeAt loc = inBounds loc && occupied board loc
    occupied = if includeSelf then isOccupied else isOccupiedBy (opponent player)

    walkLen White 1   = 2 -- can move two at start
    walkLen Black row = walkLen White (7 - row)
    walkLen _     row = 1

    dir = playerDir player

    pawnWalkPrefix = takeWhile (not . isOccupied board)

    Occupied player piece = board ! start


inBoundsCoord x = x >= 0 && x < dim

-- faster score board
scoreBoard :: Board -> Double
scoreBoard board = sum $ map (scoreLoc board) allLocs

scoreLoc board loc = scoreSquare (board ! loc) + (scoreLocActions board loc)

-- scoreBoard2 board = sum $ map (scoreLoc2 board) allLocs
-- scoreLoc2 board loc = scoreSquare (board ! loc) + (scoreLocActions board loc)

scoreLocActions board loc = case board ! loc of
  Empty                 -> 0.0
  Occupied player piece -> actionScore
    where
      actionScore = sum $ map (scoreTarget player) targets

      targets = possibleTargets board loc


      scoreTarget :: Player -> Location -> Double
      scoreTarget player target = let
          baseMobilityScore :: Double = mobilityFactor + centralityFactor * (tacticalValue target)
        in dir * case board ! target of
          Empty                             -> baseMobilityScore
          Occupied targetPlayer targetPiece -> occupiedScore
            where

              occupiedScore
                | targetPlayer == player = defenseScore
                | otherwise              = baseMobilityScore + attackScore

              attackScore  = attackFactor * targetPieceValue / actionPieceValue
              defenseScore = defenseFactor / sqrt(targetPieceValue * actionPieceValue)

              targetPieceValue = pieceValue targetPiece
              actionPieceValue = pieceValue piece
              valueRatio = targetPieceValue / actionPieceValue



      dir = playerDir player

      tacticalValue (L x y) = centrality (fromIntegral x) + progression (fromIntegral y)
      progression y = if player == White then y / 7 else 1 - (y / 7)
      centrality  x = (3.5 - abs (x - 3.5)) / 3

      centralityFactor = 0.005
      mobilityFactor   = 0.015
      attackFactor     = 0.15
      defenseFactor    = 0.08


scoreSquare (Occupied player piece) = (playerDir player) * (pieceValue piece)
scoreSquare Empty = 0.0

printBestMove :: Player -> Board -> InputT IO Board
printBestMove player board = do
  -- mapM outputStrLn $ foldl1 (++) $ map ("":) $ chunksOf 8 $ map (\l -> show l ++ " " ++ show (100 * scoreLocActions newBoard l )) (reverse allLocs)
  printBoard newBoard
  outputStrLn $ show $ scoreBoard newBoard
  return newBoard
  where
    newBoard = board // fst best
    best = bestMove player board

traceVal x = x -- trace ("VAL: " ++ show x) x

maxDepth = 3
bestMove player board = getBestMove player board score
  where
    score board = traceVal ((minimaxScore maxDepth player) board) -- + 0.05 * scoreBoard2 board

minimaxScore 0     _      board = score -- if score < -5 then trace ("OK - 0 " ++ show score) score else score 
  where score = scoreBoard board
minimaxScore depth player board = score -- trace (show depth ++ " " ++ show score) score
  where
    (_, score) = getBestMove (opponent player) board (minimaxScore (depth - 1) (opponent player))
    --scoredMoves = map (\m -> (m, minimaxScore (depth - 1) (opponent player) (board // m))) $ moves

maxBy f (x:xs) = maxBy' f x xs
maxBy' f best [] = best
maxBy' f best (x:xs) = maxBy' f newBest xs
  where newBest = if f x > f best then x else best


getBestMove :: Player -> Board -> (Board -> Double) -> (Diff, Double)
getBestMove player board scoreFunc = maxBy (\x -> dir * snd x) scoredMoves
  where
    scoredMoves = map (\m -> (m, scoreFunc (board // m))) moves
    moves = possiblePlayerMoves player board
    dir = playerDir player

--sortMoves :: Player -> [(Diff, Double)] -> [(Diff, Double)]
--sortMoves player scoredMoves = sortBy (\b a -> compare (dir * snd a) (dir * snd b)) scoredMoves
--
bound x
  | x < 0 = error "< 0"
  | x >= dim = error "> dim"
  | otherwise = x



updateBoard :: Board -> [(Location, SqState)] -> Board
updateBoard initial states = initial // states

emptyBoard :: Board
emptyBoard = listArray (L 0 0, L (dim-1) (dim-1)) (take (dim*dim) $ repeat Empty)

startingBoard = listArray (L 0 0, L 7 7) $
  [ map whiten backRow
  , map whiten frontRow
  , take (4*8) $ repeat Empty
  , map blacken frontRow
  , map blacken backRow
  ] >>= id
  where
    backRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
    frontRow = take dim $ repeat Pawn


printBoard board = outputStrLn (showBoard board)
showBoard :: Board -> String
showBoard board = (join' "\n" $ reverse numberedRows) ++ bottomLine ++ bottomLabels
  where 
    numberedRows = zipWith (\c r -> c:" | "++r) ['1'..] $ map showRow rows 
    showRow row = join' " " $ map show row
    rows = chunksOf dim (elems board)
    bottomLine = "\n  +" ++ (take (dim * 2) $ repeat '-')
    bottomLabels = "\n    " ++ (join' "" $ take dim $ map (:" ") ['a'..]) 

join' :: [a] -> [[a]] -> [a]
join' sep = foldl1 (\a b -> a ++ sep ++ b)


chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = (take n l) : (chunksOf n (drop n l))
  | otherwise = error "Negative n"

  
repl :: IO ()
repl = runInputT defaultSettings (loop startingBoard White)
  where 
    loop :: Board -> Player -> InputT IO ()
    loop board player = do
      minput <- getInputLine "% "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          -- outputStrLn $ "Input was: " ++ input
          newBoard <- printBestMove player board
          loop newBoard (opponent player)





-- convenience
a1 = L 0 0
a2 = L 0 1
a3 = L 0 2
a4 = L 0 3
a5 = L 0 4
a6 = L 0 5
a7 = L 0 6
a8 = L 0 7
b1 = L 1 0
b2 = L 1 1
b3 = L 1 2
b4 = L 1 3
b5 = L 1 4
b6 = L 1 5
b7 = L 1 6
b8 = L 1 7
c1 = L 2 0
c2 = L 2 1
c3 = L 2 2
c4 = L 2 3
c5 = L 2 4
c6 = L 2 5
c7 = L 2 6
c8 = L 2 7
d1 = L 3 0
d2 = L 3 1
d3 = L 3 2
d4 = L 3 3
d5 = L 3 4
d6 = L 3 5
d7 = L 3 6
d8 = L 3 7
e1 = L 4 0
e2 = L 4 1
e3 = L 4 2
e4 = L 4 3
e5 = L 4 4
e6 = L 4 5
e7 = L 4 6
e8 = L 4 7
f1 = L 5 0
f2 = L 5 1
f3 = L 5 2
f4 = L 5 3
f5 = L 5 4
f6 = L 5 5
f7 = L 5 6
f8 = L 5 7
g1 = L 6 0
g2 = L 6 1
g3 = L 6 2
g4 = L 6 3
g5 = L 6 4
g6 = L 6 5
g7 = L 6 6
g8 = L 6 7
h1 = L 7 0
h2 = L 7 1
h3 = L 7 2
h4 = L 7 3
h5 = L 7 4
h6 = L 7 5
h7 = L 7 6
h8 = L 7 7
