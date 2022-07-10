module Main where
import Utils 
import HamiltonianTourGenerator 
import Solver

main = do
    putStrLn "---------------------------------------------------"
    putStrLn "Welcome to the Haskell Hidato Solver and Generator!"
    putStrLn "---------------------------------------------------\n"
    putStrLn "Do you want to generate and solve \n(1) a random rectangular hidato \n(2) a custom one parsed from stdin\n(3) just solve a given hidato provided by you or\n(4) just generate a hidato given the grid? \nPlease enter a number according to an option:"
    option <- getLine
    if option == "1" 
        then do
            {
                putStrLn "Option 1 selected. Please enter two integers (one by line) representing the hidato's dimensions in rows and columns...";
                n <- getLine;
                m <- getLine;
                let{boardNM = full_k_matrix (read n) (read m) 0};  
                let{equalTo0 = positionsEqualTo0 boardNM};
                let{solvedHidato = tryGenerateSolvedHidato boardNM equalTo0};
                if solvedHidato == (full_minus_1_matrix (read n) (read m)) 
                    then 
                        putStrLn "It's not possible to construct the Hidato"
                else do
                    let{uniqueSolutionHidato = generateHidatoWithUniqueSolution (matrixToHidato solvedHidato)};
                    putStrLn "The hidato unsolved that was generated is:\n";
                    putStrLn (printMatrix (board uniqueSolutionHidato));
                    putStrLn "\nThe hidato is solved like this:\n";
                    putStrLn (printMatrix solvedHidato);
            }

    else if option == "2" 
        then do 
            putStrLn "Option 2 selected. Parsing the custom hidato grid from stdin..."
            (n,m,boardNM) <- parse;
            let{equalTo0 = positionsEqualTo0 boardNM};
            let{solvedHidato = tryGenerateSolvedHidato boardNM equalTo0};
            if solvedHidato == (full_minus_1_matrix n m) 
                then 
                    putStrLn "It's not possible to construct the Hidato"
            else do
                let{uniqueSolutionHidato = generateHidatoWithUniqueSolution (matrixToHidato solvedHidato)};
                putStrLn "The hidato unsolved that was generated is:\n";
                putStrLn (printMatrix (board uniqueSolutionHidato));
                putStrLn "\nThe hidato is solved like this:\n";
                putStrLn (printMatrix solvedHidato);
    else if option == "3" 
        then do 
            putStrLn "Option 3 selected. Parsing the custom hidato from stdin..."
            (n,m,boardNM) <- parse;
            let{equalTo0 = positionsEqualTo0 boardNM};
            let{hidatoSols = solve boardNM};
            if (length hidatoSols) == 0
                then 
                    putStrLn "It's not possible to solve the given Hidato"
            else
                putStrLn "\nThe hidato is solved like this:\n";
                putStrLn (printMatrix (hidatoSols!!0));
    else if option == "4" 
        then do 
            putStrLn "Option 4 selected. Parsing the custom hidato grid from stdin..."
            (n,m,boardNM) <- parse;
            let{equalTo0 = positionsEqualTo0 boardNM};
            let{solvedHidato = tryGenerateSolvedHidato boardNM equalTo0};
            if solvedHidato == (full_minus_1_matrix n m) 
                then 
                    putStrLn "It's not possible to construct the Hidato"
            else do
                let{uniqueSolutionHidato = generateHidatoWithUniqueSolution (matrixToHidato solvedHidato)};
                putStrLn "The hidato unsolved that was generated is:\n";
                putStrLn (printMatrix (board uniqueSolutionHidato));
    else
        putStrLn "Invalid option!"
    
    putStrLn "Ending program..."

    