import System.Random
import Control.Monad
import Data.List (foldl')
import Data.Vector (toList)
import Text.Printf (printf)
import Prelude
import Control.Parallel.Strategies

-- Define a type synonym for a matrix and vector
type Matrix a = [[a]]
type Vector a = [a]

-- Generate a random matrix with given dimensions
generateRandomMatrix :: Int -> Int -> IO (Matrix Int)
generateRandomMatrix numRows numCols = replicateM numRows (replicateM numCols (randomRIO (1, 10)))

-- Generate a random vector with given dimensions
generateRandomVector :: Int -> IO (Vector Int)
generateRandomVector numRows = replicateM numRows (randomRIO (1, 10))

-- Print a matrix
printMatrix :: Show a => Matrix a -> IO ()
printMatrix matrix = mapM_ (putStrLn . unwords . map show) matrix

-- Print a matrix with fixed precision for Double values
printDoubleMatrix :: Matrix Double -> IO ()
printDoubleMatrix matrix = mapM_ (putStrLn . unwords . map (printf "%.2f")) matrix

-- Print a vector
printVector :: Show a => Vector a -> IO ()
printVector vector = putStrLn (unwords (map show vector))

-- Print a vector with fixed precision for Double values
printDoubleVector :: Vector Double -> IO ()
printDoubleVector vector = putStrLn (unwords (map (printf "%.2f") vector))

-- Generate an augmented matrix from a matrix and a vector
generateAugmentedMatrix :: Matrix a -> Vector a -> Matrix a
generateAugmentedMatrix matrix vector = zipWith (\row b -> row ++ [b]) matrix vector

-- Convert a matrix of Int to a matrix of Double
convertMatrix :: Matrix Int -> Matrix Double
convertMatrix = map (map fromIntegral)

-- Convert an augmented matrix into an upper triangular matrix
toUpperTriangular :: Matrix Double -> Matrix Double
toUpperTriangular matrix = foldl row matrix [0 .. numRows - 1]
  where
    numRows = length matrix

    row mat rowIndex =
      let pivotRow = mat !! rowIndex
          pivot = pivotRow !! rowIndex
          rowMultiplier r =
            let factor = r !! rowIndex / pivot
            in zipWith (-) r (map (* factor) pivotRow)
      in parMap rdeepseq (\(i, r) -> if i > rowIndex then rowMultiplier r else r) (zip [0..] mat)

-- Back substitution to solve an upper triangular system
backSubstitution :: Matrix Double -> Matrix Double
backSubstitution matrix = foldl row matrix [0 .. numRows - 1]
  where
    numRows = length matrix

    row mat rowIndex = 
      let pivotRow = mat !! rowIndex
          pivot = pivotRow !! rowIndex
          rowMultiplier r =
            let factor = r !! rowIndex / pivot
            in zipWith (-) r (map (* factor) pivotRow)
      in parMap rdeepseq (\(i, r) -> if i < rowIndex then rowMultiplier r else r) (zip [0..] mat)

indexedMap :: Matrix Double -> Vector Double
indexedMap mat = zipWith (\index row -> (last row) / (row!!index)) [0..] mat

-- Generate an output vector
genOutputVector :: Matrix Double -> Vector Double
genOutputVector matrix = indexedMap matrix

main :: IO ()
main = do
    putStrLn "Enter the number of rows: "
    numRows <- readLn
    putStrLn "Enter the number of columns: "
    numCols <- readLn
    randomMatrix <- generateRandomMatrix numRows numCols
    randomVector <- generateRandomVector numRows
    putStrLn "Random Matrix:"
    printMatrix randomMatrix
    putStrLn "Random Vector:"
    printVector randomVector
    let augmented = generateAugmentedMatrix randomMatrix randomVector
    putStrLn "Augmented matrix:"
    printMatrix augmented
    let convertedAugmented = convertMatrix augmented
    let triangular = toUpperTriangular convertedAugmented
    putStrLn "Triangular matrix:"
    printDoubleMatrix triangular
    let solution = backSubstitution triangular
    putStrLn "Solutions:"
    printDoubleVector (genOutputVector solution)
