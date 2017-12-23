import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.HaLex.Dfa
import Language.HaLex.FaAsDiGraph
import Data.Char
import System.Process

data Reg = Epsilon |
      Literal Char |
      Or Reg Reg   |
      Then Reg Reg |
      Star Reg
      deriving (Eq, Show)

data RegTree = E Int |
        L Char Int |
        O RegTree RegTree Int |
        T RegTree RegTree Int |
        S RegTree Int
        deriving (Show, Eq)

reNumber :: RegTree ->  Int -> RegTree
reNumber (E n) l = E (n+l)
reNumber (L c n) l = L c (n+l)
reNumber (O t1 t2 n) l =
  let t1' = reNumber t1 l
      t2' = reNumber t2 l
  in O t1' t2' (n+l)
reNumber (T t1 t2 n) l =
  let t1' = reNumber t1 l
      t2' = reNumber t2 l
  in T t1' t2' (n+l)
reNumber (S t n) l =
  let t' = reNumber t l
  in S t' (n+l)

buildTreeHelper :: Reg -> Int -> (Int, RegTree)
buildTreeHelper Epsilon n = (1, E 1)
buildTreeHelper (Literal c) n = (1, L c 1)
buildTreeHelper (Or r1 r2) n =
  let (n1, t1) = buildTreeHelper r1 0
      (n2, t2) = buildTreeHelper r2 0
      t2' = reNumber t2 n1
      sub_tree = O t1 t2' (n1 + n2 + 1)
  in (n1 + n2 + 1, reNumber sub_tree n)
buildTreeHelper (Then r1 r2) n =
  let (n1, t1) = buildTreeHelper r1 0
      (n2, t2) = buildTreeHelper r2 0
      t2' = reNumber t2 n1
      sub_tree = T t1 t2' (n1 + n2 + 1)
  in (n1 + n2 + 1, reNumber sub_tree n)
buildTreeHelper (Star r) n =
  let (n1, t1) = buildTreeHelper r 0
      sub_tree = S t1 (n1 + 1)
  in (n1 + 1, reNumber sub_tree n)

buildTree :: Reg -> RegTree
buildTree r = t
  where (_, t) = buildTreeHelper r 0

getNodeNumber :: RegTree -> Int
getNodeNumber (E n) = n
getNodeNumber (L c n) = n
getNodeNumber (T t1 t2 n) = n
getNodeNumber (O t1 t2 n) = n
getNodeNumber (S t n) = n

buildNullableHelper :: RegTree -> Map.Map Int Bool -> Map.Map Int Bool
buildNullableHelper (E n) m = Map.insert n True m
buildNullableHelper (L c n) m = Map.insert n False m
buildNullableHelper (O t1 t2 n) m =
  let m1 = buildNullableHelper t1 m
      m2 = buildNullableHelper t2 m1
      n1 = getNodeNumber t1
      n2 = getNodeNumber t2
      b1 = lookupNullable m2 n1
      b2 = lookupNullable m2 n2
  in (Map.insert n (b1 || b2) m2)
buildNullableHelper (T t1 t2 n) m =
  let m1 = buildNullableHelper t1 m
      m2 = buildNullableHelper t2 m1
      n1 = getNodeNumber t1
      n2 = getNodeNumber t2
      b1 = lookupNullable m2 n1
      b2 = lookupNullable m2 n2
  in (Map.insert n (b1 && b2) m2)
buildNullableHelper (S t n) m =
    let m1 = buildNullableHelper t m
    in (Map.insert n True m1)

buildNullable :: RegTree -> Map.Map Int Bool
buildNullable t = buildNullableHelper t Map.empty

buildFirstHelper :: RegTree -> Map.Map Int Bool -> Map.Map Int [Int] -> Map.Map Int [Int]
buildFirstHelper (E n) nMap m = Map.insert n [] m
buildFirstHelper (L c n) nMap m = Map.insert n [n] m
buildFirstHelper (O t1 t2 n) nMap m =
  let m1 = buildFirstHelper t1 nMap m
      m2 = buildFirstHelper t2 nMap m1
      f1 = lookupFirst m2 (getNodeNumber t1)
      f2 = lookupFirst m2 (getNodeNumber t2)
  in (Map.insert n (f1 ++ f2) m2)
buildFirstHelper (T t1 t2 n) nMap m =
  let m1 = buildFirstHelper t1 nMap m
      m2 = buildFirstHelper t2 nMap m1
      f1 = lookupFirst m2 (getNodeNumber t1)
      f2 = lookupFirst m2 (getNodeNumber t2)
      b1 = lookupNullable nMap (getNodeNumber t1)
  in if(b1)
     then Map.insert n (f1 ++ f2) m2
     else Map.insert n f1 m2
buildFirstHelper (S t n) nMap m =
  let m1 = buildFirstHelper t nMap m
      f1 = lookupFirst m1 (getNodeNumber t)
  in Map.insert n f1 m1

buildFirst :: RegTree -> Map.Map Int [Int]
buildFirst t = buildFirstHelper t (buildNullable t) Map.empty

buildLastHelper :: RegTree -> Map.Map Int Bool -> Map.Map Int [Int] -> Map.Map Int [Int]
buildLastHelper (E n) nMap m = Map.insert n [] m
buildLastHelper (L c n) nMap m = Map.insert n [n] m
buildLastHelper (O t1 t2 n) nMap m =
  let m1 = buildLastHelper t1 nMap m
      m2 = buildLastHelper t2 nMap m1
      f1 = lookupLast m2 (getNodeNumber t1)
      f2 = lookupLast m2 (getNodeNumber t2)
  in Map.insert n (f1 ++ f2) m2
buildLastHelper (T t1 t2 n) nMap m =
  let m1 = buildLastHelper t1 nMap m
      m2 = buildLastHelper t2 nMap m1
      f1 = lookupLast m2 (getNodeNumber t1)
      f2 = lookupLast m2 (getNodeNumber t2)
      b2 = lookupNullable nMap (getNodeNumber t2)
  in if(b2)
     then Map.insert n (f1 ++ f2) m2
     else Map.insert n f2 m2
buildLastHelper (S t n) nMap m =
  let m1 = buildLastHelper t nMap m
      f1 = lookupLast m1 (getNodeNumber t)
  in Map.insert n f1 m1

buildLast :: RegTree -> Map.Map Int [Int]
buildLast t = buildLastHelper t (buildNullable t) Map.empty

buildFollowHelper :: RegTree -> Map.Map Int [Int] -> Map.Map Int [Int] -> Map.Map Int [Int] -> Map.Map Int [Int]
buildFollowHelper (T t1 t2 n) firstPos lastPos followPos =
  let n1 = getNodeNumber t1
      n2 = getNodeNumber t2
      last1 = lookupLast lastPos n1
      first2 = lookupFirst firstPos n2
      followPos1 = buildFollowHelper t1 firstPos lastPos followPos
      followPos2 = buildFollowHelper t2 firstPos lastPos followPos1
  in  updateMap followPos2 first2 last1
buildFollowHelper (S t1 n) firstPos lastPos followPos =
  let n1 = getNodeNumber t1
      last1 = lookupLast lastPos n1
      first1 = lookupFirst firstPos n1
      followPos1 = buildFollowHelper t1 firstPos lastPos followPos
  in updateMap followPos1 first1 last1
buildFollowHelper (O t1 t2 n) firstPos lastPos followPos =
  let followPos1 = buildFollowHelper t1 firstPos lastPos followPos
      followPos2 = buildFollowHelper t2 firstPos lastPos followPos1
  in followPos2
buildFollowHelper _ _ _ followPos = followPos

updateMap :: Map.Map Int [Int] -> [Int] -> [Int] -> Map.Map Int [Int]
updateMap followPos first (x:xs) =
  updateMap (Map.alter alterFn x followPos) first xs
  where alterFn (Just v) = Just (v ++ first)
        alterFn Nothing = Just first
updateMap followPos first [] = followPos

buildFollow :: RegTree -> Map.Map Int [Int]
buildFollow t = buildFollowHelper t (buildFirst t) (buildLast t) Map.empty

buildSymbolMapHelper :: RegTree -> Map.Map Char [Int] -> Map.Map Char [Int]
buildSymbolMapHelper (L c n) m = Map.alter alterFn c m
  where alterFn (Just v) = Just (n:v)
        alterFn Nothing = Just [n]
buildSymbolMapHelper (T t1 t2 n) m =
  let m1 = buildSymbolMapHelper t1 m
      m2 = buildSymbolMapHelper t2 m1
  in m2
buildSymbolMapHelper (O t1 t2 n) m =
  let m1 = buildSymbolMapHelper t1 m
      m2 = buildSymbolMapHelper t2 m1
  in m2
buildSymbolMapHelper (S t n) m =
  buildSymbolMapHelper t m
buildSymbolMapHelper _ m = m

buildSymbolMap :: RegTree -> Map.Map Char [Int]
buildSymbolMap t = buildSymbolMapHelper t Map.empty

symbolMapLookup :: Map.Map Char [Int] -> Char -> [Int]
symbolMapLookup m c =
  case Map.lookup c m of
    Just l -> l
    otherwise -> error "symbolMapLookup: symbol not found"

{-
Logic for DFA:
while (there is an unmarked state S in DStates)
  mark S
  for(each input symbol a)
    let U be the union of followpos(p)
      for all p that correspond to a
    if(U is not in DStates)
      add U to DStates
    Dtran[S,a] = U
-}

findAllP_to_a :: [Int] -> [Int] -> [Int]
findAllP_to_a (x:xs) aList =
  if(x `elem` aList)
  then x : (findAllP_to_a xs aList)
  else findAllP_to_a xs aList
findAllP_to_a [] aList = []

getUnion_followPos :: [Int] -> Map.Map Int [Int] -> [Int]
getUnion_followPos xs m = (Set.toList . Set.fromList) (concat union)
          where union = getUnion_Helper xs m

getUnion_Helper :: [Int] -> Map.Map Int [Int] -> [[Int]]
getUnion_Helper (x:xs) m = (lookupFollow m x) : (getUnion_Helper xs m)
getUnion_Helper [] m = []

isNewState :: [Int] -> [[Int]] -> Bool
isNewState state dStates =  if(state `elem` dStates)
                            then False else True

type Trans = ([Int], Char, [Int])

innerForLoop :: [Int] -> Map.Map Char [Int] -> Map.Map Int [Int] -> [Char] -> [[Int]] -> [Trans] -> ([[Int]], [Trans])
innerForLoop state symbolMap followPos symbols dStates dTrans =
  innerForLoopHelper state symbolMap followPos symbols dStates dTrans []

innerForLoopHelper :: [Int] -> Map.Map Char [Int] -> Map.Map Int [Int] -> [Char] -> [[Int]] -> [Trans] -> [[Int]] -> ([[Int]], [Trans])
innerForLoopHelper state symbolMap followPos (a:as) dStates dTrans accStates =
  let allCorres_a = findAllP_to_a state (symbolMapLookup symbolMap a)
      unionFollowPos = getUnion_followPos allCorres_a followPos
      bool_isNewState = isNewState unionFollowPos (dStates ++ accStates)
      newTrans = (state, a, unionFollowPos) : dTrans
  in if(bool_isNewState)
     then innerForLoopHelper state symbolMap followPos as dStates newTrans (unionFollowPos: accStates)
     else innerForLoopHelper state symbolMap followPos as dStates newTrans accStates
innerForLoopHelper state symbolMap followPos [] dStates dTrans accStates = (accStates, dTrans)

outerWhileLoopHelper :: [[Int]] -> Map.Map Char [Int] -> Map.Map Int [Int] -> [Char] -> [[Int]] -> [Trans] -> ([[Int]], [Trans])
outerWhileLoopHelper (state: states) symbolMap followPos symbols dStates dTrans =
  let (newStates, newTrans) = innerForLoop state symbolMap followPos symbols dStates dTrans
  in outerWhileLoopHelper (states ++ newStates) symbolMap followPos symbols (dStates ++ newStates) newTrans
outerWhileLoopHelper [] _ _ _ dStates dTrans = (dStates, dTrans)

outerWhileLoop :: [[Int]] -> Map.Map Char [Int] -> Map.Map Int [Int] -> [Char] -> [[Int]] -> ([[Int]], [Trans])
outerWhileLoop leftStates symbolMap followPos symbols dStates =
  outerWhileLoopHelper leftStates symbolMap followPos symbols dStates []

getStartState :: RegTree -> Map.Map Int [Int] -> [Int]
getStartState t firstPos = lookupFirst firstPos (getNodeNumber t)

buildStates_Trans :: [Int] -> Map.Map Char [Int] -> Map.Map Int [Int] -> [Char] -> ([[Int]], [Trans])
buildStates_Trans startState symbolMap followPos symbols =
  outerWhileLoop [startState] symbolMap followPos symbols [startState]

getFinalStates :: [[Int]] -> Int -> [[Int]]
getFinalStates states n = [state | state <- states, n `elem` state]

data DFA = DFA [Int] [[Int]] [Trans] [[Int]] [Char]
      deriving Show

buildDfa :: Reg -> DFA
buildDfa reg =
  let regTree = buildTree reg
      nullable = buildNullable regTree
      firstPos = buildFirst regTree
      lastPos = buildLast regTree
      followPos = buildFollow regTree
      symbolMap = buildSymbolMap regTree
      symbols = Map.keys symbolMap
      startState = getStartState regTree firstPos
      (states, trans) = buildStates_Trans startState symbolMap followPos symbols
      finalStates = getFinalStates states ((getNodeNumber regTree) - 1)
  in DFA startState states trans finalStates symbols

printDfa :: DFA -> IO()
printDfa (DFA startState states trans finalStates symbols) =
  putStr ("Initial State: \n" ++ (show startState) ++ "\n\n" ++
      "States : \n" ++ (showStates states) ++ "\n\n" ++
      "Moves: \n" ++ (showMoves trans) ++ "\n\n" ++
      "Final States: \n" ++ (showStates finalStates) ++ "\n")


showStates :: [[Int]] -> String
showStates (x:xs) = (show x) ++ "\n" ++ showStates(xs)
showStates [] = []

showMoves :: [([Int], Char, [Int])] -> String
showMoves ((state, c, finalState):xs) =
  ((show state) ++ " on " ++ [c] ++ " = " ++ (show finalState) ++ "\n") ++ (showMoves xs)
showMoves [] = []

lookupNullable :: Map.Map Int Bool -> Int -> Bool
lookupNullable m n = case (Map.lookup n m) of
                      Just b -> b
                      otherwise -> error "helper: key not found for nullable"

lookupFirst :: Map.Map Int [Int] -> Int -> [Int]
lookupFirst m n = case (Map.lookup n m) of
                    Just l -> l
                    otherwise -> error "helper: key not found for first"

lookupLast :: Map.Map Int [Int] -> Int -> [Int]
lookupLast m n = case (Map.lookup n m) of
                    Just l -> l
                    otherwise -> error "helper: key not found for last"

lookupFollow :: Map.Map Int [Int] -> Int -> [Int]
lookupFollow m n = case (Map.lookup n m) of
                    Just l -> l
                    otherwise -> []

reNumberKeys :: Map.Map Int a -> Int -> Map.Map Int a
reNumberKeys m n = Map.mapKeys (\k -> k + n) m

reNumberKV :: Map.Map Int [Int] -> Int -> Map.Map Int [Int]
reNumberKV m n = Map.map (\v -> map (\x -> x + n) v) (reNumberKeys m n)

mapUnion :: Map.Map Int a -> Map.Map Int a -> Map.Map Int a
mapUnion m1 m2 = Map.union m1 m2

prettyPrintList :: Show a => [a] -> String
prettyPrintList [] = []
prettyPrintList(x:xs) = show(x) ++ ['\n'] ++ (prettyPrintList xs)

{-
  reg -> regTree
  1. nullable
  2. first
  3. follow
  4. symbols
  5. symbol map
  6. Dfa
  7. extract start state
  8. find finish states
-}

build_graphvizDfa :: DFA -> Dfa [Int] Char
build_graphvizDfa (DFA startState states trans finalStates symbols) =
  Dfa symbols states startState finalStates delta
  where
    delta state c = concat [finalState | (initialState, symbol, finalState) <- trans, initialState == state, symbol == c]

sym :: [Char]
sym=['a','b']

sta :: [Int]
sta = [1,2,3,4]

stt :: Int
stt=1

stf::[Int]
stf=[3]

ex1  :: Dfa Int Char
ex1  = Dfa sym sta stt stf delta1
      where
        delta1 1 'a' = 1
        delta1 1 'b' = 2
        delta1 2 'a' = 3
        delta1 _ _   = 4

clex :: String -> [Token]
clex (c:cs)	| isDigit c = num_token : clex rest_num_token
		| isAlpha c = var_token : clex rest_var_token
		| otherwise = [c]:clex cs
    where
			var_token = c:takeWhile isIdChar cs
			num_token = c:takeWhile isDigit cs
			rest_num_token = dropWhile isDigit cs
			rest_var_token = dropWhile isIdChar cs
clex [] = []

isIdChar :: Char -> Bool
isIdChar c= isAlphaNum c || c=='_'

type Token = String

convert :: [Token] -> [Token]
convert [] =[]
convert [x] = [x]
convert (x:y:xs) = if(x=="\"" && y=="\"") then x:(convert xs)
			else x:(convert (y:xs))

final :: [Token]->String
final [] = []
final (x:xs) = x++(final xs)

{-

Steps to Input the regular expression:

First define literals as follows so that it becomes easy to
write larger regex.

a = Literal 'a'
b = Literal 'b'
k = Literal 'k'
e = Epsilon

e stands for Epsilon

Some Examples
1. (a|b)* -> Star (Or a b)

2. (ab| e ) c -> Then (Or (Then a b) e) c

An augmented regular expression is needed as the
algorithm demands so. The augMentedReg takes in your
regex and augments a '#' at the end.

Uncomment reg to write your own regex ->
-}

hash = Literal '#'
a = Literal 'a'
b = Literal 'b'
c = Literal 'c'
e = Epsilon
reg =  (Then (Then (Star a) (Star b)) c)
augMentedReg = Then reg hash

main = do let myDfa = buildDfa augMentedReg
          printDfa myDfa
          let dfa = build_graphvizDfa myDfa
          dfa2graphviz2file dfa "myFile"
          x <- readFile "myFile.dot"
          writeFile "myFile1.dot" (final (convert (clex x)))
          exitSuccess <- system "dot -Tpng myFile1.dot -o myImage.png"
          return()
