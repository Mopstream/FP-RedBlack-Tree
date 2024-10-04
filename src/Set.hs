module Set (
    Set,

    empty,
    singleton,

    insert,
    insertList,

    delete,
    deleteList,

    union,
    difference,
    intersection,

    toList,
    fromList,

    member,
    notMember,

    -- null,
    size,
    
    -- TESTING
    propBlackHeight,
    propBlackRoot,
    propRedChilds,

    -- DEBUG
    toDot,
)
where

import qualified RBTree
import Data.List (intersect)

type Set = RBTree.Node


empty :: Set a
empty = RBTree.empty

insert :: Ord a => a -> Set a -> Set a
insert = RBTree.insert

delete :: Ord a => a -> Set a -> Set a
delete = RBTree.delete

toList :: Set a -> [a]
toList = RBTree.toList

member :: Ord a => a -> Set a -> Bool
member = RBTree.find

null :: Set a -> Bool
null = RBTree.isNull

size :: Set a -> Int
size = RBTree.size

singleton :: Ord a => a -> Set a
singleton x = insert x empty

fromList :: Ord a => [a] -> Set a
fromList = insertList empty

notMember :: Ord a => a -> Set a -> Bool
notMember x s = not $ member x s

union :: Ord a => Set a -> Set a -> Set a
union s1 s2 = insertList s1 $ toList s2

difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = deleteList s1 $ toList s2

intersection :: Ord a => Set a -> Set a -> Set a
intersection node1 node2 = fromList $ intersect (toList node1) (toList node2)

insertList :: Ord a => Set a -> [a] -> Set a
insertList = foldr insert

deleteList :: Ord a => Set a -> [a] -> Set a
deleteList = foldl $ flip delete

instance Foldable Set where
    foldr f acc set = foldr f acc (toList set)

instance Ord a => Semigroup (Set a) where
    (<>) = union

instance Ord a => Monoid (Set a) where
    mempty = empty

instance Ord a => Eq (Set a) where
    (==) s1 s2 = (==) (toList s1) (toList s2)

instance Ord a => Ord (Set a) where
    compare s1 s2 = compare (toList s1) (toList s2)


{-- 
            in RBTree

instance (Show a, Ord a) => Show (Set a)

instance Functor Set

--}



--    TESTING
propBlackHeight :: Set a -> Bool
propBlackHeight = RBTree.blackHeight

propBlackRoot :: Set a -> Bool
propBlackRoot = RBTree.blackRoot

propRedChilds :: Set a -> Bool
propRedChilds = RBTree.redChilds

--  debug
toDot :: Set Int -> String
toDot = RBTree.toDot