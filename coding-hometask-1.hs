import Data.List (sortBy)
import Data.Ratio

able_to_pack :: [Rational] -> [Rational] -> [Rational] -> Bool
able_to_pack [] _ _ = True
able_to_pack _ [] _ = False
able_to_pack (w:ws) (b:bs) bw = (w <= b) && able_to_pack ws (bw ++ [b - w] ++ bs) []
				|| able_to_pack (w:ws) bs (bw ++ [b])


solve_bp_decision :: [Rational] -> Int -> Bool
solve_bp_decision weights n = able_to_pack weights (replicate n 1) []


solve_bp_evaluation :: [Rational] -> Int
solve_bp_evaluation weights = (head $ filter (solve_bp_decision weights) [0..])


bins_num :: [[Rational]] -> Int
bins_num bins = solve_bp_evaluation $ map (sum) bins


join_two_bins :: [Rational] -> [[Rational]] -> [[Rational]] -> [[Rational]]
join_two_bins _ [] _ = error "Somethig went totally wrong!"
join_two_bins current_bin (b:bs) bw = let other_bins = bs ++ bw
				      in if (sum (current_bin ++ b) <= (1 :: Rational)) &&
					 ((bins_num $ other_bins ++ [current_bin] ++ [b])
					 == (bins_num $ other_bins ++ [current_bin ++ b]))
					 then bw ++ [current_bin ++ b] ++ bs
					 else join_two_bins current_bin bs (bw ++ [b])


join_all_bins :: [[Rational]] -> [[Rational]] -> [[Rational]]
join_all_bins bins_left@(b:bs)  bins_finished
	| length bins_left > bins_num bins_left = if (bins_num bs < bins_num bins_left)
					     then join_all_bins bs (b:bins_finished)
					     else join_all_bins (join_two_bins b bs []) bins_finished
	| otherwise = bins_left ++ bins_finished


solve_bp_search :: [Rational] -> [[Rational]]
solve_bp_search weights = join_all_bins [ [x] | x <- (sortBy (flip compare) weights)] []


