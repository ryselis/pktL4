import Data.List

main = do
    file <- readFile "data"
    let oilPatches = words $ removeTwoFirst file
    let oilPatchPositions = map (oilPositions []) oilPatches
    let mappedOilPatchPositions = enumerate oilPatchPositions
    let flattenedOilPatchPositions =  concat mappedOilPatchPositions
    print $ countOilPatches flattenedOilPatchPositions 0

removeTwoFirst :: String -> String
removeTwoFirst(x) = unwords $ tail $ tail $ words x

oilPositions :: [Int] -> String -> [Int]
oilPositions i "" = []
oilPositions i xs = if head xs == '*' then oilPositions i (tail xs) else i ++ [length (tail xs)] ++ oilPositions i (tail xs)

enumerate :: [[Int]] -> [[(Int, Int)]]
enumerate xs = map enumerateInner (zip [1..] xs)

enumerateInner :: (Int, [Int]) -> [(Int, Int)]
enumerateInner xs = zip ([fst xs | x <- snd xs]) (snd xs)

removeAdjacentOil :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
removeAdjacentOil base [] = []
removeAdjacentOil base xs = do
    let adjacent = filter (\k -> (abs $ (fst k) - (fst $ base)) <= 1 && (abs $ (snd k) - (snd $ base)) <= 1) xs
    let remainder = xs \\ adjacent
    let stuff = [removeAdjacentOil a remainder | a <- adjacent]
    foldr intersect remainder stuff

countOilPatches :: [(Int, Int)] -> Int -> Int
countOilPatches [] c = c
countOilPatches xs c = countOilPatches (removeAdjacentOil (head xs) xs) (c+1)
