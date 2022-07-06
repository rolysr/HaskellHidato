module Utils (find_value, updateMatrix, updateList, dirs, validPosition, full_minus_1_matrix, markPositions, bfs, isConnected, Pos, Hidato) where
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude

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

maxMat :: [[Int]] -> Int
maxMat [] = -1000000
maxMat a = maximum [(maximum (a!!i)) | i<-[0..((length a)-1)]]

minMat :: [[Int]] -> Int
minMat [] = 1000000
minMat a = minimum [(minimum (a!!i)) | i<-[0..((length a)-1)]]