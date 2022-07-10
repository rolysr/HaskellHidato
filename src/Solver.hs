module Solver (
    solve
) where
import Utils

-- Returns 0, 1 or 2 solutions for the hidato puzzle
solve :: [[Int]] -> [[[Int]]]
solve a = phases_solve (phase_find a) a

avaliable_info :: [[Int]] -> Int -> [(Int,[Int])]
avaliable_info a x = do
    let pos = find_value a x
    if pos == [] 
        then if maxMat a >= x
            then avaliable_info a (x+1)
            else []
        else [(x,[pos!!0,pos!!1])] ++ avaliable_info a (x+1)

transform :: [(Int,[Int])] -> [(Int,[Int],[Int])]
transform l = [((fst (l!!i))-(fst (l!!(i-1))),snd (l!!(i-1)),snd (l!!i)) | i<-[1..((length l)-1)]]

phase_find :: [[Int]] -> [(Int,[Int],[Int])]
phase_find a = transform (avaliable_info a 1)

phases_solve :: [(Int,[Int],[Int])] -> [[Int]] -> [[[Int]]]
phases_solve phases a = backtrack (snd3 (phases!!0)) (trd3 (phases!!0)) 0 phases a 0

less_than_x_to_minus_1 :: [[Int]] -> Int -> [[Int]]
less_than_x_to_minus_1 a x = [[ if ((a!!i!!j)/=0 && (a!!i!!j)<x) then -1 else (a!!i!!j) |j<-[0..((length (a!!0))-1)]]|i<-[0..((length a)-1)]]

-- Tries to backtrack in every possible direction
try_all :: [Int] -> [Int] -> Int -> [(Int,[Int],[Int])] -> [[Int]] -> Int -> [[Int]] -> [[[Int]]]
try_all [x0,y0] [x1,y1] index phases a total_solutions [] = []
try_all [x0,y0] [x1,y1] index phases a total_solutions ([dx,dy]:dir_s) = do
    let x = x0+dx
    let y = y0+dy
    if total_solutions==2 || ((validPosition x y (length a) (length (a!!0))) && ((a!!x!!y)==0))
        then do 
            let solutions = backtrack [x,y] [x1,y1] index phases (updateMatrix a x y ((a!!x0!!y0)+1)) total_solutions 
            solutions ++ (try_all [x0,y0] [x1,y1] index phases a (total_solutions+(length solutions)) dir_s)
        else try_all [x0,y0] [x1,y1] index phases a total_solutions dir_s

-- backtrack args:
-- <actual position of the backtracking> 
-- <ending position of the current phase> 
-- <index of the current phase> 
-- <phases>
-- <actual state of matrix>
-- <number of solutions>
backtrack :: [Int] -> [Int] -> Int -> [(Int,[Int],[Int])] -> [[Int]] -> Int -> [[[Int]]]
backtrack [x0,y0] [x1,y1] index phases a total_solutions = 
    -- Base stopping cases
    if (total_solutions==2) || (not (isConnected (less_than_x_to_minus_1 a (a!!x0!!y0)))) || ((maximum [abs(x0-x1),abs(y0-y1)]) > ((a!!x1!!y1)-(a!!x0!!y0))) 
        then []
        -- Base Successful case
        else if (index==((length phases)-1)) && (adjacent_positions [x0,y0] [x1,y1]) && (((a!!x1!!y1)-(a!!x0!!y0))==1)
            then [a]
            -- Backtracking cases
            else if (adjacent_positions [x0,y0] [x1,y1]) && (((a!!x1!!y1)-(a!!x0!!y0))==1)
                -- The next phase
                then backtrack (snd3 (phases!!(index+1))) (trd3 (phases!!(index+1))) (index+1) phases a total_solutions
                -- Try the adjacents 
                else try_all [x0,y0] [x1,y1] index phases a total_solutions dirs