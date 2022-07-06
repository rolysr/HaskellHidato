module HamiltonianTourGenerator (
    generateSolvedHidato, 
    hamiltonianPath
) where
import Utils

--Returns if possible a Hidato with a board as a Hamiltonian path starting at a given position given the size of the matrix
generateSolvedHidato :: [[Int]] -> Int -> Int -> Pos -> Hidato
generateSolvedHidato [] _ _ _ = NilHidato
generateSolvedHidato matrix n m p | (hamiltonian_path_board) == (full_minus_1_matrix n m) = NilHidato
                                  | otherwise = Hidato {board = hamiltonian_path_board, posMin = (getPosMinBoardGZ hamiltonian_path_board), posMax = (getPosMaxBoardGZ hamiltonian_path_board), minValue = (getMinValueBoardGZ hamiltonian_path_board), maxValue = (getMaxValueBoardGZ hamiltonian_path_board), width = m, height = n}
                                  where hamiltonian_path_board = (HamiltonianPath matrix n m p)
--Generate a hamiltonian path in
hamiltonianPath :: [[Int]] -> Int -> Int -> Pos -> [[Int]]
hamiltonianPath [] _ _ _= []
hamiltonianPath board n m p | (board || (not isConnected board)) == (full_minus_1_matrix n m) = board
                            | (board!!(row(p))!!column(p) == 0 && length (number_positions_distinct_from_minus_1 board) == 1) = (sum_1_positions_distinct_from_minus_1 board) 
                            | otherwise = (tryWhileNotGetValidHamBoard (hamiltonianPath newBoard) possibleAdyacents n m)
                            where newBoard = (updateMatrix board row(p) column(p) -1)
                                  possibleAdyacents = map covertListToPosition (adjacents_avaliable_single [row(p), column(p)] board dirs)
