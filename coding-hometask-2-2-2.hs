import Data.Set (Set, member, fromList, toList, difference, insert, delete)
import qualified Data.Map.Strict as Map 
import Data.List (minimumBy, sort)
import System.Environment
import System.IO
import System.CPUTime (getCPUTime)
import Control.DeepSeq (deepseq)


type Graph a = Map.Map a [a]


numberOfEdges :: (Ord a) => Graph a -> Set a -> Int
numberOfEdges graph segment = foldl (\acc list -> acc + listWeight list) 0 potentialEdges
    where weightOfEdge v
              | not (v `member` segment) = 1
              | otherwise                = 0
          
          potentialEdges = map (\v -> (Map.lookup v graph)) (toList segment)
          listWeight Nothing = 0
          listWeight (Just list) = sum $ map weightOfEdge list


nothingToList :: Maybe [a] -> [a]
nothingToList Nothing = []
nothingToList (Just list) = list


getNeighbourhood :: (Ord a) => Graph a -> (Int, Set a) -> [(Int, Set a)]
getNeighbourhood graph (weight, segment) = [(weight + (if v1 `elem` (nothingToList $ Map.lookup v2 graph)
                                            then 2 else 0) +
                                            n1 + n2, swapVerticles segment v1 v2) |
                                            (v1, n1) <- sdList, (v2, n2) <- adList]
    where addition = difference (Map.keysSet graph) segment
          swapVerticles set v1 v2 = insert v2 $ delete v1 set
          neighboursInSegment v set = length $ filter (\x -> x `member` set) (nothingToList $ Map.lookup v graph)
          segmentList = toList segment
          additionList = toList addition
          sdList = zip segmentList (map (\v -> (neighboursInSegment v segment) -
                   (neighboursInSegment v addition)) segmentList)
          adList = zip additionList (map (\v -> (neighboursInSegment v addition) -
                   (neighboursInSegment v segment)) additionList)


variableDepthLocalSearch :: (Ord a) => Graph a -> [a]
variableDepthLocalSearch graph = toList $ depthSearch startingPoint startingPoint depth depth startingNE startingNE
    where depthSearch originalSegment currentSegment originalDepth currentDepth originalNE currentNE
              | currentNE < originalNE = depthSearch currentSegment currentSegment originalDepth originalDepth currentNE currentNE
              | currentDepth > 0       = depthSearch originalSegment bestNeighbour originalDepth (currentDepth - 1) originalNE neighbourNE
              | otherwise               = originalSegment
              where (neighbourNE, bestNeighbour) = minimumBy (\(n1, _) (n2, _) -> compare n1 n2) (getNeighbourhood graph (currentNE, currentSegment))

          startingPoint = fromList $ take ((length $ Map.keys graph) `div` 2) (sort $ Map.keys graph)
          depth         = 1
          startingNE    = numberOfEdges graph startingPoint


files = ["add20.graph", "cti.graph", "m14b.graph", "t60k.graph"]


readGraphFromFile :: (Integral a, Ord a, Read a) => String -> IO (Graph a)
readGraphFromFile file = do
    text <- readFile file
    let n_verticles = read . head . words . head . lines $ text
    let verticles = [1,2..(n_verticles + 1)]
    let list = map words (tail $ lines text)
    let neighbours = map (map read) list
    return (Map.fromList $ zip verticles neighbours)


getWorkTime :: (Integral a, Ord a) => Graph a -> IO (Double, Int)
getWorkTime graph = do
    startTime <- getCPUTime
    let crossingEdgesNum = numberOfEdges graph (fromList $ variableDepthLocalSearch graph)
    endTime <- crossingEdgesNum `deepseq` getCPUTime
    return (fromInteger (endTime - startTime) / 1e12, crossingEdgesNum)


runFile :: String -> IO ()
runFile file = do
    graph <- readGraphFromFile file
    (time, numOfEdges) <- getWorkTime graph
    putStrLn ("solving instance " ++ file ++ "... done in " ++ show(time) ++ "seconds with quality " ++ show(numOfEdges))


runAllFiles :: [String] -> IO ()
runAllFiles = sequence_ . map runFile


main = do
    runAllFiles files
