import Data.List

-- nonUniques f pairs = 
--     map head $ filter (\lst -> length lst > 1) $ group $ sort $ map f pairs
nonUniques f = map head . filter ((> 1) . length) . group . sort . map f 

-- filterNonUniques
filterNU f lst = filter (\p -> f p `elem` (nonUniques f lst)) lst

pairs :: [(Int, Int)]
pairs = [(i * j, i + j) | i <- [1..9], j <- [1..9], i >= j]

filterP = filterNU fst
filterS = filterNU snd
filterPS = filterS . filterP

pairsP1 = filterP pairs
pairsS1 = filterS pairsP1
-- etc
-- or...

pairsS4' = filterPS $ filterPS $ filterPS $ filterPS pairs
-- or 
pairsS4'' = last $ take 5 $ iterate filterPS pairs

-- Answer is 8 and 2
