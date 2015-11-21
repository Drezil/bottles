module Main where

{- given n bottles and k boxes with capacity c_i (i ∈ 1..l, l ∈ ℕ)
 - return the number of combinations to put the n bottles into the k
 - boxes -}

import Text.Read (readMaybe)

type Bottles = Int
type Capacity = Int
type Box = (Capacity, Bottles)


main :: IO ()
main = do
        n <- readInt "How many Bottles?"
        b <- readInt "How many Boxes?"
        c <- readCombos b
        putStrLn $ "There are " ++ show (length (validCombos n c)) ++ " combinations for " ++ show n ++ " bottles in those " ++ show b ++ " boxes."

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

-- faster, but not that elegant.
{-allCombos :: Int -> [Capacity] -> [[Box]]
allCombos n c = allCombos' n c [[]]
  where
    allCombos' :: Int -> [Capacity] -> [[Box]] -> [[Box]]
    allCombos' _ [] x = x
    allCombos' n (x:xs) l = allCombos' n xs [ a:ls  | a <- fillBox x, ls <- l, stillValidBoxes n (a:ls)]-}

allCombos :: [Capacity] -> [[Box]]
allCombos = traverse fillBox

validBoxes :: Int -> [Box] -> Bool
validBoxes n b = sum (snd <$> b) == n

stillValidBoxes :: Int -> [Box] -> Bool
stillValidBoxes n b = sum (snd <$> b) <= n

validCombos :: Int -> [Capacity] -> [[Box]]
validCombos n c = filter (validBoxes n) $ allCombos c
