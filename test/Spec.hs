import qualified Data.List as L
import Set
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Testing RBTree properties ..."
  quickCheck (withMaxSuccess 1000 propSet)
  putStrLn "\nTesting Monoid laws 1, 2 ..."
  quickCheck (withMaxSuccess 1000 propMonoidMempty)
  putStrLn "\nTesting Monoid law 3 ..."
  quickCheck (withMaxSuccess 1000 propMonoidAssociativity)
  putStrLn "\nTesting Set to List ..."
  quickCheck (withMaxSuccess 1000 toListTest)
  putStrLn "\nTesting Inserting ..."
  quickCheck (withMaxSuccess 1000 propInsert)
  putStrLn "\nTesting Deleting ..."
  quickCheck (withMaxSuccess 1000 propDelete)
  putStrLn "\nTesting Size ..."
  quickCheck (withMaxSuccess 1000 propSize)
  putStrLn ""
  assertEquals (null empty, True) "Testing Null ..."
  assertEquals (toList $ fromList [3, 2, 1], [1, 2, 3]) "Testing Sorting ..."
  assertEquals (sum $ fromList [1, 2, 3, 4, 5], sum [1, 2, 3, 4, 5]) "Testing Fold ..."
  assertEquals ((* 5) <$> fromList [1, 2, 3, 4, 5], fromList $ (* 5) <$> [1, 2, 3, 4, 5]) "Testing Functor ..."
  assertEquals (toList $ fromList "4321", "1234") "Testing Polymorphic on Chars ..."
  assertEquals (toList $ fromList [(1, 'b'), (1, 'a'), (2, 'c')], [(1, 'a'), (1, 'b'), (2, 'c')]) "Testing Polymorphic on Tuples ..."
  putStrLn "\nAll Done!"

propSet :: [Int] -> Bool
propSet list = all ($ fromList list) [propBlackHeight, propBlackRoot, propRedChilds]

propMonoidMempty :: [Int] -> Bool
propMonoidMempty list = all (== set) [mappend mempty set, mappend set mempty]
  where
    set = fromList list

propMonoidAssociativity :: [Int] -> [Int] -> [Int] -> Bool
propMonoidAssociativity s1 s2 s3 = ((s1 `mappend` s2) `mappend` s3) == (s1 `mappend` (s2 `mappend` s3))

toListTest :: [Int] -> Bool
toListTest list = toList (fromList list) == L.sort (L.nub list)

propInsert :: [Int] -> Bool
propInsert list = and tested
  where
    tested = map (\set -> all ($ set) [propBlackHeight, propBlackRoot, propRedChilds]) sets
    sets :: [Set Int]
    sets = scanl (flip insert) empty list

propDelete :: [Int] -> Bool
propDelete list = and tested
  where
    tested = map (\set -> all ($ set) [propBlackHeight, propBlackRoot, propRedChilds]) sets
    sets :: [Set Int]
    sets = scanl (flip delete) (fromList list) list

propSize :: [Int] -> Bool
propSize list = size (fromList list) == length (L.nub list)

assertEquals :: (Eq a) => (a, a) -> String -> IO ()
assertEquals (a, b) testDesc =
  putStrLn $ (if a == b then "Passed: " else "Failed: ") <> testDesc
