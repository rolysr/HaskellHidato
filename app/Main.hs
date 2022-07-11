module Main where
import Utils 
import HamiltonianTourGenerator 
import Solver

main = do
    putStrLn "---------------------------------------------------"
    putStrLn "Welcome to the Haskell Hidato Solver and Generator!"
    putStrLn "---------------------------------------------------\n"
    putStrLn "Do you want to generate and solve a random rectangular hidato (1) or a custom one (2) parsed from stdin? Please enter a number according to an option:"
    option <- getLine
    if option == "1" 
        then do
            {
                putStrLn "Option 1 selected. Please enter two integers (one by line) representing the hidato's dimensions in rows and columns...";
                n <- getLine;
                m <- getLine;
                let{boardNM = full_k_matrix (read n) (read m) 0};  
                let{equalTo0 = positionsEqualTo0 boardNM};
                let{p = selectRandomPosFrom equalTo0};
                let{solvedHidato = tryGenerateSolvedHidato boardNM (validHidatoPositions boardNM)};
                if solvedHidato == full_minus_1_matrix n m 
                    then 
                        putStrLn "It's not possible to construct the Hidato"
                else
                    let{uniqueSolutionHidato = generateHidatoWithUniqueSolution solvedHidato};
                    putStrLn "The hidato unsolved that was generated is:\n";
                    putStrLn (printMatrix (board uniqueSolutionHidato));
                    putStrLn "\nThe hidato is solved like this:\n";
                    putStrLn (printMatrix (board solvedHidato));
            }

    else if option == "2" 
        then do 
            putStrLn "Option 2 selected. Parsing the custom hidato from stdin..."
            (n,m,boardNM) <- parse;
            let{equalTo0 = positionsEqualTo0 boardNM};
            let{p = selectRandomPosFrom equalTo0};
            let{solvedHidato = generateSolvedHidato boardNM (length boardNM) (length (boardNM!!0)) p};
            if solvedHidato == full_minus_1_matrix n m 
                then 
                    putStrLn "It's not possible to construct the Hidato"
            else
                let{uniqueSolutionHidato = generateHidatoWithUniqueSolution solvedHidato};
                putStrLn "The hidato unsolved that was generated is:\n";
                putStrLn (printMatrix (board uniqueSolutionHidato));
                putStrLn "\nThe hidato is solved like this:\n";
                putStrLn (printMatrix (board solvedHidato));
    else
        putStrLn "Invalid option!"
    
    putStrLn "Ending program..."

    