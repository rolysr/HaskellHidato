import Utils

-- Returns 0, 1 or 2 solutions for the hidato puzzle
solve :: [[Int]] -> [[[Int]]]
solve a = phase_solve (phase_find a) a

avaliable_info :: [[Int]] -> [(Int,[Int])]
avaliable_info a x = 
    if find

phase_find :: [[Int]] -> [(Int,[Int])]
phase_find a = transform (avaliable_info a 1)

phase_solve :: [(Int,[Int])] -> [[Int]] -> [[[Int]]]
phase_solve phases a = []