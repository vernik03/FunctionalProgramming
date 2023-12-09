import System.Random
import Control.Monad
import Data.List (foldl')
import Data.Vector (toList)
import Text.Printf (printf)
import Prelude
import Control.Parallel.Strategies

type Matrix a = [[a]]
type Vector a = [a]

makeRandomMatrix :: Int -> Int -> IO (Matrix Int)
makeRandomMatrix numRows numCols = replicateM numRows (replicateM numCols (randomRIO (1, 10)))

makeRandomVector :: Int -> IO (Vector Int)
makeRandomVector numRows = replicateM numRows (randomRIO (1, 10))

printMatrix :: Show a => Matrix a -> IO ()
printMatrix matrix = mapM_ (putStrLn . unwords . map show) matrix

printDoubleMatrix :: Matrix Double -> IO ()
printDoubleMatrix matrix = mapM_ (putStrLn . unwords . map (printf "%.2f")) matrix

printVector :: Show a => Vector a -> IO ()
printVector vector = putStrLn (unwords (map show vector))

printDoubleVector :: Vector Double -> IO ()
printDoubleVector vector = putStrLn (unwords (map (printf "%.2f") vector))

makeAugmentedMatrix :: Matrix a -> Vector a -> Matrix a
makeAugmentedMatrix matrix vector = zipWith (\row b -> row ++ [b]) matrix vector

convertMatrix :: Matrix Int -> Matrix Double
convertMatrix = map (map fromIntegral)

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

makeOutputVector :: Matrix Double -> Vector Double
makeOutputVector matrix = indexedMap matrix

main :: IO ()
main = do
    putStrLn "Number of rows: "
    numRows <- readLn
    putStrLn "Number of columns: "
    numCols <- readLn
    randomMatrix <- makeRandomMatrix numRows numCols
    randomVector <- makeRandomVector numRows    
    putStrLn "Random Matrix:"
    printMatrix randomMatrix
    putStrLn "Random Vector:"
    printVector randomVector
    let augmented = makeAugmentedMatrix randomMatrix randomVector
    putStrLn "Augmented matrix:"
    printMatrix augmented
    let convertedAugmented = convertMatrix augmented
    let triangular = toUpperTriangular convertedAugmented
    putStrLn "Triangular matrix:"
    printDoubleMatrix triangular
    let solution = backSubstitution triangular
    putStrLn "Results:"
    printDoubleVector (makeOutputVector solution)
