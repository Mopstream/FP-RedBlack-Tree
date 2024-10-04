module Main (main) where

import Set

main :: IO ()
main = do
--   print $ deleteList (fromList [0, -3, 2, -4, -1, 3, 1, 4, 6, -5, 7, 5, -2, 8, -6]) [0, -3, 2, -4]
  putStrLn $ toDot $ deleteList (fromList [0, -3, 2, -4, -1, 3, 1, 4, 6, -5, 7, 5, -2, 8, -6]) [0, -3, 2, -4, -1, 3, 1, 4, 6, -5]
