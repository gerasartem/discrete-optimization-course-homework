import Data.Set (Set, member, fromList, toList, difference, insert, delete)
import System.Environment
import System.IO


type Graph a = ([a], [(a, a)])


numberOfEdges :: (Ord a) => Graph a -> Set a -> Int
numberOfEdges (verticles, edges) segment = foldl (\acc e -> acc + crossingEdge e) 0 edges
    where crossingEdge (v1, v2)
              | (v1 `member` segment) && not (v2 `member` segment) = 1
              | not (v1 `member` segment) && (v2 `member` segment) = 1
              | otherwise = 0


getNeighbourhood :: (Ord a) => Graph a -> Set a -> [Set a]
getNeighbourhood (verticles, edges) segment = [swapVerticles segment v1 v2 | v1 <- toList segment, v2 <- toList addition]
    where addition = difference (fromList verticles) segment
          swapVerticles set x y = insert y $ delete x set


argMin :: (Ord b) => (a -> b) -> [a] -> Maybe a
argMin f [] = Nothing
argMin f list@(x:_) =
    let compareArgs a b 
            | f a <= f b = a
            | otherwise  = b
    in  Just (foldl (\acc e -> compareArgs acc e) x list)


basicLocalSearch :: (Ord a) => Graph a -> ([a], [a], Int)
basicLocalSearch ([], _) = ([], [], 0)
basicLocalSearch graph@(verticles, edges) = betterSegmentSearch (Just (fromList (take ((length verticles) `div` 2) verticles)))
    where betterSegmentSearch Nothing = ([], [], 0)
          betterSegmentSearch (Just segment)
              | bestNumberOfEdges bestSegment < currentNumberOfEdges = betterSegmentSearch bestSegment
              | otherwise = (toList segment, toList addition, currentNumberOfEdges)
              where currentNumberOfEdges = numberOfEdges graph segment
                    bestSegment = argMin (numberOfEdges graph) (getNeighbourhood graph segment)
                    bestNumberOfEdges Nothing = 0
                    bestNumberOfEdges (Just x) = numberOfEdges graph x
                    addition = difference (fromList verticles) segment

readVerticles :: [String] -> [Int]
readVerticles (line:tail)
    | line !! 0 == 'p' = [1..read (last $ init $ words line)]
    | otherwise = readVerticles tail


readEdges :: [String] -> [(Int, Int)]
readEdges text = 
    let getEdge acc line
            | line !! 0 == 'e' = (read $ last $ init (words line), read $ last (words line)):acc
            | otherwise = acc
    in foldl (\acc line -> getEdge acc line) [] text


main = do
       args <- getArgs
       handle <- openFile (args !! 0) ReadMode
       content <- hGetContents handle
       let verticles = readVerticles (lines content)
       let edges = readEdges (lines content)
       print (basicLocalSearch (verticles, edges))
       hClose handle
