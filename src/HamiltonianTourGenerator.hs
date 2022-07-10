module HamiltonianTourGenerator (
    generateSolvedHidato, 
    hamiltonianPath,
    generateHidatoWithUniqueSolution,
) where
import Utils
import Solver

--Returns if possible a Hidato with a board as a Hamiltonian path starting at a given position given the size of the matrix
generateSolvedHidato :: [[Int]] -> Int -> Int -> Pos -> Hidato
generateSolvedHidato [] _ _ _ = NilHidato
generateSolvedHidato matrix n m p | (hamiltonian_path_board) == (full_minus_1_matrix n m) = NilHidato
                                  | otherwise = (matrixToHidato hamiltonian_path_board)
                                  where hamiltonian_path_board = (hamiltonianPath matrix n m p)
--Generate a hamiltonian path in
hamiltonianPath :: [[Int]] -> Int -> Int -> Pos -> [[Int]]
hamiltonianPath matrix n m p | ( (not (isConnected matrix))) || matrix == (full_minus_1_matrix n m) = matrix
                            | (matrix!!(row p)!!(column p) == 0 && (number_positions_distinct_from_minus_1 matrix) == 1) = (sum_1_positions_distinct_from_minus_1 matrix) 
                            | otherwise = (tryWhileNotGetValidHamBoard (hamiltonianPath newBoard n m) possibleAdyacents n m)
                            where newBoard = (updateMatrix matrix (row p) (column p) (-1))
                                  possibleAdyacents = map covertListToPosition (adjacents_avaliable_single [(row p), (column p)] matrix dirs)

generateHidatoWithUniqueSolution :: Hidato -> Hidato
generateHidatoWithUniqueSolution NilHidato = NilHidato
generateHidatoWithUniqueSolution h = matrixToHidato ((deletePositionRandomWhileUniqueSolution (board h) positions)) where positions = validHidatoPositionsToDelete (board h)


deletePositionRandomWhileUniqueSolution :: [[Int]] -> [Pos] -> [[Int]]
deletePositionRandomWhileUniqueSolution [] _ = []
deletePositionRandomWhileUniqueSolution board positions | (length (solve newBoard) == 1) = (deletePositionRandomWhileUniqueSolution newBoard newPositions)
                                                        | otherwise = board
                                                        where newp = selectRandomPosFrom positions
                                                              newBoard = (updateMatrix board (row newp) (column newp) 0)
                                                              newPositions = deletePositionFrom positions newp
