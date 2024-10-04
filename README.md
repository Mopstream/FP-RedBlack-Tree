# Лабораторная работа 2

Вариант: rb-set

Автор: Голиков Андрей Сергеевич P34092 335126

Цель: освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing).

## Реализация

Полный код представлен в `src`. Ниже продемонстрированы наиболее важные участки кода

### Структура дерева

```haskell
data Color = Red | Black deriving (Show, Eq)

data Node a
  = Node
      { key :: a,
        left :: Node a,
        right :: Node a,
        tempCOLOR :: Color
      }
  | Null
```

### Вставка

```haskell
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
```
(Это не самое страшное, самое страшное впереди...)

### Удаление

```haskell
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
```

### Обертка Set

```haskell
type Set = RBTree.Node
```

### Реализация класса типов Monoid

```haskell
instance Ord a => Semigroup (Set a) where
    (<>) = union

instance Ord a => Monoid (Set a) where
    mempty = empty
```

### Реализация класса типов Foldable

```haskell
instance Foldable Set where
    foldr f acc set = foldr f acc (toList set)
```

### Реализация класса типов Functor

```haskell
instance Functor Node where
  fmap _ Null = Null
  fmap f (Node k l r c) = Node (f k) (fmap f l) (fmap f r) c
```

## Тестирование

### Код некоторых тестов

```haskell
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

```
### Отчет тестирования

```
lab2> test (suite: lab2-test)
            
Progress 1/2: lab2Testing RBTree properties ...
+++ OK, passed 1000 tests.

Testing Monoid laws 1, 2 ...
+++ OK, passed 1000 tests.

Testing Monoid law 3 ...
+++ OK, passed 1000 tests.

Testing Set to List ...
+++ OK, passed 1000 tests.

Testing Inserting ...
+++ OK, passed 1000 tests.

Testing Deleting ...
+++ OK, passed 1000 tests.

Testing Size ...
+++ OK, passed 1000 tests.

Passed: Testing Null ...
Passed: Testing Sorting ...
Passed: Testing Fold ...
Passed: Testing Functor ...
Passed: Testing Polymorphic on Chars ...
Passed: Testing Polymorphic on Tuples ...

All Done!
                  


lab2> Test suite lab2-test passed
Completed 2 action(s).
```

## Вывод

Красно-черное дерево на иммутабельных данных это кошмар и ужас. Столько времени дебажить не до конца подвешанные вершинки...
Но +- мне понравилось, самая интересная лаба из актуальных на сегодня. Не согласен с требованием реализовать `map`, так как для 2/3 вариантов (set и bag) отображение противопоказано, нам могут подсунуть функцию, которая поломает всякую сортировку. Единственный выход - требовать от функции монотонного возрастания (`a > b => f(a) > a(b)`), но такого никакой инструмент языка гарантировать не может.
