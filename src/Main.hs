module Main where

{- given n bottles and k boxes with capacity c_i (i ∈ 1..l, l ∈ ℕ)
 - return the number of combinations to put the n bottles into the k
 - boxes
 - -}

import Text.Read (readMaybe)

type Bottles = Int
type Capacity = Int
type Box = (Capacity, Bottles)


main :: IO ()
main = do
        n <- readInt "How many Bottles?"
        b <- readInt "How many Boxes?"
        c <- readCombos b
        print $ length $ validCombos n c

readInt :: String -> IO Int
readInt q = do
                putStrLn q
                bstr <- getLine
                case readMaybe bstr of
                  Just a -> return a
                  Nothing -> do
                               putStrLn $ "could not parse: "++bstr
                               readInt q

readCombos :: Int -> IO [Capacity]
readCombos = readCombos' 1
   where
     readCombos' :: Int -> Int -> IO [Capacity]
     readCombos' i 0 = return []
     readCombos' i a = do
                 x <- readInt $ "Capacity for Box " ++ show i ++ "?"
                 xs <- readCombos' (i+1) (a-1)
                 return (x:xs)

fillBox :: Capacity -> [Box]
fillBox c = [(c,k) | k <- [0..c]]

allCombos :: [Capacity] -> [[Box]]
allCombos c = allCombos' c [[]]
  where
    allCombos' :: [Capacity] -> [[Box]] -> [[Box]]
    allCombos' [] x = x
    allCombos' (x:xs) l = allCombos' xs [ a:ls  | a <- fillBox x, ls <- l]

validBoxes :: Int -> [Box] -> Bool
validBoxes n b = sum (snd <$> b) == n

validCombos :: Int -> [Capacity] -> [[Box]]
validCombos n c = filter (validBoxes n) $ allCombos c
