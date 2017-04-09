import Data.Set (Set, member, fromList, toList, difference, insert, delete, isSubsetOf, empty)
import System.Environment
import System.IO
import System.CPUTime (getCPUTime)
import Control.DeepSeq (deepseq)

type Graph a = ([a], [(a, a)])



numberOfEdges :: (Ord a) => Graph a -> Set a -> Int
numberOfEdges (verticles, edges) segment = foldl (\acc edge -> acc + weightOfEdge edge) 0 edges
    where weightOfEdge (v1, v2)
              | (v1 `member` segment) && not (v2 `member` segment) = 1
              | not (v1 `member` segment) && (v2 `member` segment) = 1
              | otherwise = 0


getNeighbourhood :: (Ord a) => Graph a -> Set a -> [Set a]
getNeighbourhood (verticles, edges) segment = [swapVerticles segment v1 v2 | v1 <- toList segment, v2 <- toList addition]
    where addition = difference (fromList verticles) segment
          swapVerticles set v1 v2 = insert v2 $ delete v1 set


argMin :: (Ord b) => (a -> b) -> [a] -> a
argMin f (x:xs) = 
    let compareArgs a b
            | f a <= f b = a
            | otherwise  = b
    in foldl (\acc e -> compareArgs acc e) x xs


variableDepthLocalSearch :: (Ord a) => Graph a -> [a]
variableDepthLocalSearch graph@(verticles, edges) = toList $ depthSearch starting_point starting_point depth depth
     where depthSearch original_segment current_segment original_depth current_depth
               | current_edges_number < original_edges_number = depthSearch current_segment current_segment original_depth original_depth
               | current_depth > 0                            = depthSearch original_segment best_neighbour original_depth (current_depth - 1)
               | otherwise                                    = original_segment
               where original_edges_number = numberOfEdges graph original_segment
                     current_edges_number  = numberOfEdges graph current_segment
                     best_neighbour        = argMin (numberOfEdges graph) (getNeighbourhood graph current_segment)

           starting_point        = fromList $ take ((length verticles) `div` 2) verticles
           depth                 = 2


files = ["add20.graph", "cti.graph", "m14b.graph", "t60k.graph"]


readGraphFromFile :: String -> IO (Graph Integer)
readGraphFromFile file = do
    text <- readFile file
    let n_verticles = read . head . words . head . lines $ text
    let verticles = [1,2..(n_verticles + 1)]
    let list = zip verticles (map words (tail $ lines text))
    let edges = foldl (\acc1 (u, words) -> foldl (\acc2 word -> insert (makeOrdered(u, read word)) acc2) acc1 words) (fromList []) list
    return (verticles, toList edges) 
    where makeOrdered (x, y)
              | x < y     = (x, y)
              | otherwise = (y, x)

          
 
getWorkTime graph = do
                    start_time <- getCPUTime
                    let crossing_edges_number = numberOfEdges graph (fromList $ variableDepthLocalSearch graph)
                    end_time <- crossing_edges_number `deepseq` getCPUTime
                    return (fromInteger (end_time - start_time) / 1e12, crossing_edges_number)


runFile :: String -> IO ()
runFile file = do
               graph <- readGraphFromFile file
               (time, num_of_edges) <- getWorkTime graph
               putStrLn ("Solving instance " ++ file ++ "...done in " ++ show(time) ++ "seconds with quality " ++ show(num_of_edges))


runAllFiles :: [String] -> IO ()
runAllFiles = sequence_ . map runFile

main = do
       runAllFiles files
