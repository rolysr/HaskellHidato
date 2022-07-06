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
less_than_x_to_minus_1 a x = [[ if (a!!i!!j)<x then -1 else (a!!i!!j) |j<-[0..((length (a!!0))-1)]]|i<-[0..((length a)-1)]]

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
    if total_solutions==2 || (not (isConnected (less_than_x_to_minus_1 a (a!!x0!!y0)))) || ((maximum [abs(x0-x1),abs(y0-y1)]) < ((a!!x1!!y1)-(a!!x0!!y0))) 
        then []
        -- Base Successful case
        else if (index==((length phases)-1)) && 