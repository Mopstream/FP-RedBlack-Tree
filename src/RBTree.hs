module RBTree
  ( -- type
    Node,
    empty,
    insert,
    delete,
    find,
    size,
    isNull,
    -- convert
    toList,
    toDot,
    -- testing
    blackHeight,
    blackRoot,
    redChilds,
  )
where

import qualified Data.Graph.Inductive.Graph as GIG
import qualified Data.Graph.Inductive.PatriciaTree as PT
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes as A
import qualified Data.GraphViz.Printing as P
import qualified Data.Text.Lazy as L

data Color = Red | Black deriving (Show, Eq)

data Node a
  = Node
      { key :: a,
        left :: Node a,
        right :: Node a,
        tempCOLOR :: Color
      }
  | Null

instance Functor Node where
  fmap _ Null = Null
  fmap f (Node k l r c) = Node (f k) (fmap f l) (fmap f r) c

color :: Node a -> Color
color Null = Black
color (Node _ _ _ c) = c

instance (Show a, Ord a) => Show (Node a) where
  show node = show' node 0
    where
      show' (Node k l r c) dept = replicate (4 * dept) ' ' ++ show c ++ " " ++ show k ++ "\n" ++ show' l (dept + 1) ++ show' r (dept + 1)
      show' Null _ = ""

hang :: (Ord a) => Node a -> Node a -> Node a
hang Null _ = error "try to hang to Null node"
hang _ Null = error "try to hang Null"
hang p n
  | key n == key p = p
  | key n < key p = p {left = n}
  | key n > key p = p {right = n}
hang _ _ = error "unreachable"

isNull :: Node a -> Bool
isNull Null = True
isNull _ = False

hangByKey :: (Ord a) => Node a -> Node a -> a -> Node a
hangByKey p c k
  | k < key p = p {left = c}
  | k >= key p = p {right = c}
hangByKey _ _ _ = error "unreachable"

killChild :: (Ord a) => a -> Node a -> Node a
killChild _ Null = error "killing child of Null"
killChild a node
  | a < key node = node {left = Null}
  | a >= key node = node {right = Null}
killChild _ _ = error "unreachable pattern killChild"

empty :: Node a
empty = Null

insert :: (Ord a) => a -> Node a -> Node a
insert newKey root = case unsafeFind newKey root of
  (True, _) -> root
  (False, path) -> fix new path
  where
    new = Node newKey Null Null Red

    fix :: (Ord a) => Node a -> [Node a] -> Node a
    fix n [] = n {tempCOLOR = Black}
    fix n [p] = (hang p n) {tempCOLOR = Black}
    fix n@(Node _ _ _ Black) ancestors = foldl (flip hang) n ancestors
    fix n ancestors@((Node _ _ _ Black) : _) = (foldl (flip hang) n ancestors) {tempCOLOR = Black}
    fix n (p : g : ancestors) = case color uncle of
      Red -> fix redCaseG ancestors
      Black -> case (key n < key p, key p < key g) of
        (False, False) -> fix ffCase ancestors
        (False, True) -> fix ftCase ancestors
        (True, False) -> fix tfCase ancestors
        (True, True) -> fix ttCase ancestors
        where
          ffCase = parent {left = g {right = left p, tempCOLOR = Red}, tempCOLOR = Black}
          ttCase = parent {right = g {left = right p, tempCOLOR = Red}, tempCOLOR = Black}
          ftCase = n {tempCOLOR = Black, left = p {right = left n}, right = g {tempCOLOR = Red, left = right n}}
          tfCase = n {tempCOLOR = Black, right = p {left = right n}, left = g {tempCOLOR = Red, right = left n}}
      where
        redCaseG = (hang (hang g (parent {tempCOLOR = Black})) (uncle {tempCOLOR = Black})) {tempCOLOR = Red}
        parent = hang p n
        uncle = if key p < key g then right g else left g

delete :: (Ord a) => a -> Node a -> Node a
delete _ Null = Null
delete k root@(Node kRoot Null Null _)
  | k == kRoot = Null
  | otherwise = root
delete k root = case unsafeFind k root of
  (False, _) -> root
  (True, n : path) -> chooseNode n path
  (_, _) -> error "unreachable delete case"
  where
    chooseNode node ancestors = case node of
      Node kToDel Null Null Red -> (foldl (flip hang) (killChild kToDel (head ancestors)) (tail ancestors)) {tempCOLOR = Black}
      Node kToDel Null Null Black -> fix Null (killChild kToDel (head ancestors) : tail ancestors) kToDel
      Node _ l Null _ -> chooseNode l (l {tempCOLOR = color node} : ancestors)
      Node _ Null r _ -> chooseNode r (r {tempCOLOR = color node} : ancestors)
      Node kToDel _ _ _ ->
        let nextNode : addPath = snd $ unsafeFind kToDel (right node)
         in chooseNode nextNode (addPath ++ node {key = key nextNode} : ancestors)
      Null -> error "unreachable Null on chooseNode"

    fix n [] _ = n
    fix n (p : ancestors) unbKey = case color brother of
      Black -> case (color fstSon, color sndSon) of
        (_, Red) -> foldl (flip hang) anyRed ancestors
        (Red, _) -> fix n (redBlack : ancestors) unbKey
        (Black, Black) ->
          if color p == Red
            then foldl (flip hang) blackBlack ancestors
            else fix (hangByKey blackBlack n unbKey) ancestors (key p)
        where
          anyRed = hang (hang (brother {tempCOLOR = color p}) (sndSon {tempCOLOR = Black})) ((hangByKey newP fstSon (key brother)) {tempCOLOR = Black})
          newP = hangByKey p n unbKey
          redBlack = hang p (hang (fstSon {tempCOLOR = Black}) ((hangByKey brother rbNewHead (key fstSon)) {tempCOLOR = Red}))
          rbNewHead = if key fstSon < key p then left fstSon else right fstSon
          blackBlack = hang (newP {tempCOLOR = Black}) (brother {tempCOLOR = Red})
      Red -> fix n (newP : newG : ancestors) unbKey
      where
        (brother, fstSon, sndSon) =
          if unbKey < key p
            then (right p, left brother, right $ right p)
            else (left p, right brother, left $ left p)
        newP = hang (p {tempCOLOR = Red}) fstSon
        newG = hang (brother {tempCOLOR = Black}) newP

toList :: Node a -> [a]
toList root = toList' [] [root]
  where
    toList' acc [] = acc
    toList' acc (node : rest) = case node of
      Null -> toList' acc rest
      Node k l Null _ -> toList' (k : acc) (l : rest)
      Node k l r _ -> toList' acc (r : Node k Null Null Black : l : rest)

find :: (Ord a) => a -> Node a -> Bool
find k root = fst $ unsafeFind k root

size :: Node a -> Int
size node = size' 0 [node]
  where
    size' acc (n : rest) = case n of
      Null -> size' acc rest
      Node _ l r _ -> size' (acc + 1) (l : r : rest)
    size' acc [] = acc

unsafeFind :: (Ord a) => a -> Node a -> (Bool, [Node a])
unsafeFind k = unsafeFind' []
  where
    unsafeFind' path Null = (False, path)
    unsafeFind' path n = case compare k (key n) of
      EQ -> (True, newPath)
      LT -> unsafeFind' newPath (left n)
      GT -> unsafeFind' newPath (right n)
      where
        newPath = n : path

toGig :: Node Int -> PT.Gr Color ()
toGig x = uncurry GIG.mkGraph $ help x ([], [])
  where
    help :: Node Int -> ([GIG.LNode Color], [GIG.LEdge ()]) -> ([GIG.LNode Color], [GIG.LEdge ()])
    help Null (n, e) = (n, e)
    help (Node tk l r c) (n, e) = case l of
      Null -> (rn, re)
      (Node lk _ _ _) -> help l (rn, (tk, lk, ()) : re)
      where
        (rn, re) = case r of
          Null -> ((tk, c) : n, e)
          (Node rk _ _ _) -> help r ((tk, c) : n, (tk, rk, ()) : e)

toDot :: Node Int -> String
toDot g = L.unpack $ P.renderDot $ G.toDot $ G.graphToDot params $ toGig g
  where
    params =
      G.nonClusteredParams
        { G.isDirected = True,
          G.fmtNode = \n -> case snd n of
            Red -> [A.fontColor G.Red]
            Black -> [A.fontColor G.Black]
        }

blackHeight :: Node a -> Bool
blackHeight root = all (== firstLen) restLen
  where
    firstLen : restLen = blackHeight' [] [(root, 0)]

    blackHeight' :: [Int] -> [(Node a, Int)] -> [Int]
    blackHeight' heights [] = heights
    blackHeight' heights (n : rest) = case n of
      (Null, h) -> blackHeight' ((h + 1) : heights) rest
      (Node _ l r Black, h) -> blackHeight' heights ((l, h + 1) : (r, h + 1) : rest)
      (Node _ l r Red, h) -> blackHeight' heights ((l, h) : (r, h) : rest)


blackRoot :: Node a -> Bool
blackRoot root = color root == Black

redChilds :: Node a -> Bool
redChilds Null = True
redChilds (Node _ l r Black) = redChilds l && redChilds r
redChilds (Node _ l r Red) = color l == Black && color r == Black && redChilds l && redChilds r

