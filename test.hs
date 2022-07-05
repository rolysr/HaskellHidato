apply :: [[Int]] -> Int -> Int -> Int -> [[Int]]
apply a x1 y1 v = [[ if i==x1 && j==y1 then v else a !! i !! j | j<-[0..(length (a!!0))-1] ] | i<-[0..(length a)-1] ]

dirs :: [[Int]]
dirs=[[-1,-1],[-1,0],[-1,1],[0,-1],[0,1],[1,-1],[1,0],[1,1]]