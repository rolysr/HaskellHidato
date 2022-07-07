module Utils (
    adjacent_positions,
    trd3,
    trd4,
    trd5,
    fst3,
    fst4,
    fst5,
    snd3,
    snd4,
    snd5,
    th4_4,
    th4_5, 
    minMat, 
    maxMat, 
    find_value, 
    updateMatrix, 
    updateList, 
    dirs, 
    validPosition, 
    full_minus_1_matrix, 
    markPositions, 
    bfs, 
    isConnected, 
    Pos(..), 
    Hidato(..),
    getPosMinBoardGZ,
    getPosMaxBoardGZ,
    getMinValueBoardGZ,
    getMaxValueBoardGZ,
    number_positions_distinct_from_minus_1,
    sum_1_positions_distinct_from_minus_1,
    tryWhileNotGetValidHamBoard,
    covertListToPosition,
    adjacents_avaliable_single,
    full_k_matrix,
    printList,
    printMatrix) 
    where

import Data.Set (Set)
import qualified Data.Set as Set
import Prelude
import System.Random

updateMatrix :: [[a]] -> Int -> Int -> a -> [[a]]
updateMatrix a x y v = 
    [
        if i==x then updateList (a!!i) y v
        else 
            a !! i
        | i<-[0..(length a)-1] 
    ]

updateList :: [a] -> Int -> a -> [a]
updateList a x v = 
    [
        if i==x then v 
        else 
            a !! i
        | i<-[0..(length a)-1] 
    ]

dirs :: [[Int]]
dirs=[[-1,-1],[-1,0],[-1,1],[0,-1],[0,1],[1,-1],[1,0],[1,1]]

--Position record type
data Pos = Pos {row :: Int, column :: Int} | NilPos deriving (Show)

--Hidato record type
data Hidato = Hidato {board :: [[Int]], posMin :: Pos, posMax :: Pos, minValue :: Int, maxValue :: Int, width :: Int, height :: Int} | NilHidato deriving (Show)

validPosition :: Int -> Int -> Int -> Int -> Bool
validPosition x y n m = x>=0 && x<n && y>=0 && y<m

full_minus_1_matrix :: Int -> Int -> [[Int]]
full_minus_1_matrix n m = 
    [
        [
            -1
            | j<-[0..(m-1)]
        ]
        | i<-[0..(n-1)]
    ]

full_k_matrix :: Int -> Int -> Int -> [[Int]]
full_k_matrix n m k = 
    [
        [
            k
            | j<-[0..(m-1)]
        ]
        | i<-[0..(n-1)]
    ]

position_distinct_from_minus_1_row :: [Int] -> Int -> [Int]
position_distinct_from_minus_1_row [] x = []
position_distinct_from_minus_1_row a x = 
    if (head a) /= -1 
        then [x] 
        else position_distinct_from_minus_1_row (tail a) (x+1)

position_distinct_from_minus_1 :: [[Int]] -> Int -> [[Int]]
position_distinct_from_minus_1 [] x = [] 
position_distinct_from_minus_1 a x = 
    if position_distinct_from_minus_1_row (head a) 0 /= []
        then [[x,(position_distinct_from_minus_1_row (head a) 0)!!0]]
        else position_distinct_from_minus_1 (tail a) (x+1)

markPositions :: [[Int]] -> [[Int]] -> [[Int]]
markPositions [] a = a
markPositions (p:ps) a = markPositions ps (updateMatrix a (p!!0) (p!!1) (-1))

adjacents_avaliable_single :: [Int] -> [[Int]] -> [[Int]] -> [[Int]]
adjacents_avaliable_single [x,y] a [] = []
adjacents_avaliable_single [x,y] a ([dx,dy]:ds) = 
    if (validPosition (x+dx) (y+dy) (length a) (length (a!!0))) && a!!(x+dx)!!(y+dy)/=(-1)
        then ([x+dx,y+dy]:adjacents_avaliable_single [x,y] a ds)
        else adjacents_avaliable_single [x,y] a ds

adjacents_avaliable :: [[Int]] -> [[Int]] -> [[Int]]
adjacents_avaliable [] a = []
adjacents_avaliable (p:ps) a = (adjacents_avaliable_single p a (dirs)) ++ (adjacents_avaliable ps a)

bfs_part2 :: [[Int]] -> [[Int]] -> [[Int]]
bfs_part2 mk a = bfs (Set.elems (Set.fromList (adjacents_avaliable mk a))) a

bfs :: [[Int]] -> [[Int]] -> [[Int]]
bfs [] a = a
bfs mk a = bfs_part2 mk (markPositions mk a)

---Function to check if the graph is connected or not
isConnected :: [[Int]] -> Bool
isConnected a = full_minus_1_matrix (length a) (length (a!!0)) == bfs (position_distinct_from_minus_1 a 0) a

find_value :: [[Int]] -> Int -> [Int]
find_value a v = findValue a v 0

findValue :: [[Int]] -> Int -> Int -> [Int]
findValue [] v x = []
findValue a v x = 
    if findValue_row (head a) v 0 /= []
        then [x,(findValue_row (head a) v 0)!!0]
        else findValue (tail a) v (x+1)

findValue_row :: [Int] -> Int -> Int -> [Int]
findValue_row [] v x = []
findValue_row (p:ps) v x = 
    if p == v 
        then [x]
        else findValue_row ps v (x+1)

--Methods for getting the position of maximum and minimum elements of a matrix greater than 0
getPosMinBoardGZ :: [[Int]] -> Pos
getPosMinBoardGZ [] = NilPos
getPosMinBoardGZ l = (Pos (fst p) (snd p)) where m = getMinValueBoardGZ l
                                                 p = ([(i,j) | i <- [0..((length l)-1)] , j <- [0..((length (l !! 0)) -1)] , l!!i!!j == m] !! 0)
                                               
getPosMaxBoardGZ :: [[Int]] -> Pos
getPosMaxBoardGZ [] = NilPos
getPosMaxBoardGZ l = (Pos (fst p) (snd p)) where m = getMaxValueBoardGZ l
                                                 p = ([(i,j) | i <- [0..((length l)-1)] , j <- [0..((length (l !! 0)) -1)] , l!!i!!j == m] !! 0)
    
--Methods for getting the maximum and minimum elements of a matrix greater than 0
getMinValueBoardGZ :: [[Int]] -> Int  
getMinValueBoardGZ [] = 0
getMinValueBoardGZ l = minimumGZ (map minimumGZ l)

getMaxValueBoardGZ :: [[Int]] -> Int  
getMaxValueBoardGZ [] = 0
getMaxValueBoardGZ l = maximum (map maximum l)

minimumGZ :: [Int] -> Int
minimumGZ [] = -1
minimumGZ l | ((length newL) > 0) = (minimum newL)
            | otherwise = -1
            where newL = (filter (>0) l)

number_positions_distinct_from_minus_1 :: [[Int]] -> Int
number_positions_distinct_from_minus_1 [] = 0
number_positions_distinct_from_minus_1 matrix = sum (filter (>0) (map length (map (filter (>0)) matrix)))

sum_1_positions_distinct_from_minus_1 :: [[Int]] -> [[Int]]
sum_1_positions_distinct_from_minus_1 [] = []
sum_1_positions_distinct_from_minus_1 l = map sum_1_to_gz_list l 

sum_1_to_gz_list :: [Int] -> [Int]
sum_1_to_gz_list [] = []
sum_1_to_gz_list (x:xs) = (((\x -> if x > 0 then x+1 else x) x) : sum_1_to_gz_list xs)

tryWhileNotGetValidHamBoard :: (Pos -> [[Int]]) -> [Pos] -> Int -> Int -> [[Int]]
tryWhileNotGetValidHamBoard _ [] n m = (full_minus_1_matrix n m)
tryWhileNotGetValidHamBoard f (p:ps) n m | (newBoard /= (full_minus_1_matrix n m)) = newBoard
                                         | otherwise = tryWhileNotGetValidHamBoard f ps n m
                                         where newBoard = (f p) 

covertListToPosition :: [Int] -> Pos
covertListToPosition [] = NilPos
covertListToPosition [x] = NilPos
covertListToPosition (x:xs) = Pos {row = x, column = (head xs)} 

maxMat :: [[Int]] -> Int
maxMat [] = -1000000
maxMat a = maximum [(maximum (a!!i)) | i<-[0..((length a)-1)]]

minMat :: [[Int]] -> Int
minMat [] = 1000000
minMat a = minimum [(minimum (a!!i)) | i<-[0..((length a)-1)]]

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

fst4 :: (a,b,c,d) -> a
fst4 (a,b,c,d) = a

fst5 :: (a,b,c,d,e) -> a
fst5 (a,b,c,d,e) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

snd4 :: (a,b,c,d) -> b
snd4 (a,b,c,d) = b

snd5 :: (a,b,c,d,e) -> b
snd5 (a,b,c,d,e) = b

trd3 :: (a,b,c) -> c
trd3 (a,b,c) = c

trd4 :: (a,b,c,d) -> c
trd4 (a,b,c,d) = c

trd5 :: (a,b,c,d,e) -> c
trd5 (a,b,c,d,e) = c

th4_4 :: (a,b,c,d) -> d
th4_4 (a,b,c,d) = d

th4_5 :: (a,b,c,d,e) -> d
th4_5 (a,b,c,d,e) = d

adjacent_positions :: [Int] -> [Int] -> Bool
adjacent_positions [x0,y0] [x1,y1] = (abs(x0-x1)<=1 && abs(y0-y1)<=1)

printMatrix ::(Show a) => [[a]] -> [Char]
printMatrix [] = ""
printMatrix (x:xs) = printList x ++ "\n" ++ (printMatrix xs)

printList :: (Show a) => [a] -> [Char]
printList [] = ""
printList (x:xs) = (show x) ++ " " ++ (printList xs)
