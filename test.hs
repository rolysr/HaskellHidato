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

validPosition :: Int -> Int -> Int -> Int -> Bool
validPosition x y n m = x>=0 && x<n && y>=0 && y<m