import Data.List (minimumBy, delete, splitAt, elemIndex)
import Data.Char (isDigit)
import Control.DeepSeq (deepseq)
import System.CPUTime (getCPUTime)
import Data.Ord (comparing)
import Data.Maybe (fromJust, catMaybes)


type Point a = (a, a)


euclideanDistance :: (Floating a) => Point a -> Point a -> a
euclideanDistance (x1, y1) (x2, y2) = sqrt $ ((x1- x2)^2 + (y1 - y2)^2)


calculateTourLength :: (Floating a) => [Point a] -> a
calculateTourLength tour = sum $ [euclideanDistance x y | (x, y) <- zip tour shifted_tour] 
    where shifted_tour = (last tour) : (init tour)


findNearestPoint :: (Floating a, Ord a) => Point a -> [Point a] -> Point a
findNearestPoint x list = minimumBy comparator list
    where comparator a b = compare (euclideanDistance a x) (euclideanDistance b x)


solveTspNearestNeighbour :: (Floating a, Ord a) => [Point a] -> [Point a]
solveTspNearestNeighbour [] = []
solveTspNearestNeighbour (x:[]) = [x]
solveTspNearestNeighbour (x:xs) = x : (solveTspNearestNeighbour (nearest : (delete nearest xs)))
    where nearest = findNearestPoint x xs


coolFindNearestPoint :: (Floating a, Ord a) => [(Point a, Point a)] -> [Point a] -> Point a
coolFindNearestPoint list1 list2 = snd $ minimumBy comparator [(fst x, y) | x <- list1, y <- list2]
        where comparator (x1, y1) (x2, y2) = compare (euclideanDistance x1 y1) (euclideanDistance x2 y2)


insertPoint :: (Floating a, Ord a) => Point a -> [(Point a, Point a)] -> [(Point a, Point a)]
insertPoint x tour = part1 ++ [(a, x), (x, b)] ++ part2
    where comparator (a1, b1) (a2, b2) = compare ((euclideanDistance a1 x) + (euclideanDistance b1 x)
                                                  - (euclideanDistance a1 b1))
                                                  ((euclideanDistance a2 x) + (euclideanDistance b2 x)
                                                  - (euclideanDistance a2 b2))
          edge_to_replace@(a, b)        = minimumBy comparator tour
          partition pivot begining (x:end)
              | x == pivot = (reverse begining, end)
              | otherwise  = partition pivot (x:begining) end
          (part1, part2) = partition edge_to_replace [] tour


tourExpansion :: (Floating a, Ord a) => ([(Point a, Point a)], [Point a]) -> ([(Point a, Point a)], [Point a])
tourExpansion (tour, []) = (tour, [])
tourExpansion (tour, addition) = tourExpansion (new_tour, new_addition)
    where nearest_point = coolFindNearestPoint tour addition
          new_addition = delete nearest_point addition
          new_tour = insertPoint nearest_point tour


solveTspNearestInsertion :: (Floating a, Ord a) => [Point a] -> [Point a]
solveTspNearestInsertion [] = []
solveTspNearestInsertion (x:[]) = []
solveTspNearestInsertion (x:xs) = [a | (a, b) <- fst (tourExpansion (starting_tour, starting_addition))]
    where nearest_point = findNearestPoint x xs
          starting_tour = [(x, nearest_point), (nearest_point, x)]
          starting_addition = delete nearest_point xs


files = ["pr107.tsp", "pr152.tsp", "pr439.tsp", "d198.tsp", "d493.tsp", "d657.tsp", "d2103.tsp"]


readTspLine :: String -> Maybe (Point Double)
readTspLine line
    | isDigit $ head line = Just (read first, read second) 
    | otherwise = Nothing
    where first = head $ tail $ words line
          second = last $ words line


readTspText :: String -> [Point Double]
readTspText text = catMaybes $ map readTspLine (lines text)


readTspFile :: String -> IO [Point Double]
readTspFile file = do
                   text <- readFile file
                   return (readTspText text)

getWorkTime :: (Integral a) => ([Point Double] -> [Point Double]) -> [Point Double] -> IO (Double, a)
getWorkTime findTour points = do
                             startTime <- getCPUTime
                             let tourLength = calculateTourLength $ findTour points
                             endTime <- tourLength `deepseq` getCPUTime
                             return (fromInteger (endTime - startTime) / 1e12 , round tourLength)


runFile :: [String] -> IO ()
runFile file = do
                points <- readTspFile file 
                (nearest_neighbour_time, nearest_neighbour_length) <- getWorkTime solveTspNearestNeighbour points
                (nearest_insertion_time, nearest_insertion_length) <- getWorkTime solveTspNearestInsertion points
                putStrLn (file ++ " done in " ++ (show nearest_neighbour_time) ++ " seconds with tour length "
                          ++ (show nearest_neighbour_length) ++ " using NN and in "
                          ++ (show nearest_insertion_time) ++ " seconds with tour length "
                          ++ (show nearest_insertion_length) ++ " using NI")


runAllFiles :: [String] -> IO ()
runAllFiles files = sequence_ . map runFile files 


main = do
       runAllFiles files
