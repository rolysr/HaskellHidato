module Main where
import Utils 
import HamiltonianTourGenerator 
import Solver

main = do
    putStrLn "---------------------------------------------------"
    putStrLn "Welcome to the Haskell Hidato Solver and Generator!"
    putStrLn "---------------------------------------------------\n"
    putStrLn "Do you want to generate and solve a random rectangular hidato (1) or a custom one (2) parsed from a hidato.txt located at the same folder of this program executable? Please enter a number according to an option:"
    option <- getLine
    if option == "1" 
        then do
            {
                putStrLn "Option 1 selected. Please enter two integers (one by line) representing the hidato's dimensions in rows and columns...";
                n <- getLine;
                m <- getLine;
                let{board = full_k_matrix (read n) (read m) 0};  
                putStrLn (printMatrix board)  
            }
             
    else if option == "2" 
        then do 
            putStrLn "Option 2 selected. Parsing the custom hidato from hidato.txt file..."
    else
        putStrLn "Invalid option!"
    
    putStrLn "Ending program..."

    