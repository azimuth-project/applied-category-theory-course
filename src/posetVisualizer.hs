import Diagrams.Backend.SVG.CmdLine

{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude
import Data.List
import Data.Ord (comparing)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Colour.SRGB (sRGB24read)
import Data.Typeable

import qualified Math.Combinatorics.Poset as PS
import qualified Math.Combinat.Partitions.Set as SetPart
import qualified Data.Set as Set
import qualified PosetFunctionality as PF

--This is for the example where looking at subsets of [0..n] with n<10
-- so to display them they are being concatenated together into a single integer
data Subset = Subset Int [Int]

addDigit :: Int -> Int -> Int
addDigit num d = 10*num+d

name :: Subset -> Int
name (Subset x elts) = foldl addDigit 0 elts
name2 :: [Int] -> Int
name2 elts = foldl addDigit 0 elts

(Subset _ elts1) `isSubset` (Subset _ elts2) = all (`elem` elts2) elts1

subsetsBySize :: Int -> [[Subset]]
subsetsBySize n = map (map (Subset n))
                . groupBy ((==) `on` length)
                . sortBy (comparing length)
                . subsequences
                $ [1..n]

--length of interval is not correct, deprecated way of assigning level to each object of ps
level :: PS.Poset t -> t -> t -> Int
level ps bottom current = length $ PS.interval ps (bottom,current)

-- finds the minimum length of getting to bottom and that is where the diagram should put this object
level2 :: Eq t => PS.Poset t -> t -> t -> Int
level2 (PS.Poset (set,po)) bottom current = maximum $ 0:[1+ level2 (PS.Poset (set,po)) bottom x | x<- set, x `po` current, x /= current]

--put bottom at level 0, stuff that has nothing in between it and bottom at level 1, et cetera
breakPosetByLevel :: Eq t => PS.Poset t -> t -> [[t]]
breakPosetByLevel (PS.Poset (set,po)) bottom = groupBy (\x y -> (myLevel x == myLevel y)) $ sortBy (\x y -> (compare (myLevel x) (myLevel y))) set
                                              where myLevel = (\z -> (level2 (PS.Poset (set,po)) bottom z))

node :: Show t => t -> Diagram B
node n = text (show n) # fontSizeL 0.2 # fc white <> square 1 # fc blue

drawElts n elts = hcat
                . map (\i -> if i `elem` elts
                               then node i
                               else strutX 1
                      )
                $ [1..n]

drawSet (Subset n elts) = (drawElts n elts # centerXY
                            <> rect (fromIntegral n + 0.5) 1.5
                                 # dashingG [0.2,0.2] 0
                                 # lw thin
                                 # named (name2 elts)
                          )
drawItem n = node n # named n

hasseRow = centerX . hcat' (with & sep .~ 2) . map drawSet

hasseDiagram n = setsD # centerXY
  where setsD = vcat' (with & sep .~ fromIntegral n)
              . map hasseRow
              . reverse
              $ subsets
        subsets = subsetsBySize n

hasseDiagram2 ps bottom = setsD # centerXY
    where setsD = vcat' (with & sep .~ fromIntegral 1)
           . map (centerX . hcat' (with & sep .~ 2) . map drawItem)
           $ breakPosetByLevel ps bottom

toConnect :: Int -> [(Int,Int)]
toConnect n = concat $ zipWith connectSome (subsetsBySize n) (tail $ subsetsBySize n)
connectSome :: [Subset] -> [Subset] -> [(Int,Int)]
connectSome subs1 subs2 = [ (name s1, name s2) | s1 <- subs1
                                             , s2 <- subs2
                                             , s1 `isSubset` s2 ]

withConnections n = (hasseDiagram n) # applyAll [connectOutside' (with & gaps       .~ small
                          & headLength .~ local 0.15) j k | (j,k) <- toConnect n]

--take the poset and list of objects from two layers and put in the arrows
-- these will end up being the adjacent layers, but that is not enforced yet
toConnect2:: Eq t => PS.Poset t -> t -> [(t,t)]
toConnect2 ps bottom = concat $ zipWith (connectSome2 ps) (broken) (tail $ broken) where broken=(breakPosetByLevel ps bottom)
connectSome2 :: PS.Poset t -> [t] -> [t] -> [(t,t)]
connectSome2 (PS.Poset (set,po)) layeri layerj = [ (s1, s2) | s1 <- layeri
                                             , s2 <- layerj
                                             , s1 `po` s2 ]
withConnections2 ps bottom = (hasseDiagram2 ps bottom) # applyAll [connectOutside' (with & gaps       .~ small
                          & headLength .~ local 0.15) j k | (j,k) <- toConnect2 ps bottom]

--partition Logic
compareSetPartitions :: [[Int]] -> [[Int]] -> Bool
compareSetPartitions sp1 sp2 = and [isXSubset (Set.fromList x) sp1 | x <- sp2 ] where 
                               isXSubset x1 sp1' = or [ Set.isSubsetOf x1 (Set.fromList y) | y <- sp1' ]
partitionPoset :: Int -> PS.Poset [[Int]]
partitionPoset n = PS.Poset (map SetPart.fromSetPartition (SetPart.setPartitions n), compareSetPartitions)

--Set,Par,Rel etc
-- add the others to finish off the 16, but need their names for display purpose
-- To put into visualizer replace example with relSubCatsExample in main
data RelSubCats = CatRel | CatPar | CatSet | CatInj | CatSurj | CatBij | CatParSurj | CatParInj deriving (Read,Eq,Show,Ord,Enum,Typeable)
instance IsName RelSubCats where
                        toName a = toName $ fromEnum a
allRelSubCats :: [RelSubCats]
allRelSubCats = [CatRel,CatPar,CatSet,CatInj,CatSurj,CatBij,CatParInj,CatParSurj]
-- is it total, co-total, deterministic co-deterministic
relSubCatsPOHelper :: RelSubCats -> [Bool]
relSubCatsPOHelper CatBij = [True,True,True,True]
relSubCatsPOHelper CatInj = [True,False,True,True]
relSubCatsPOHelper CatSurj = [True,True,True,False]
relSubCatsPOHelper CatSet = [True,False,True,False]
relSubCatsPOHelper CatParInj = [False,False,True,True]
relSubCatsPOHelper CatParSurj = [False,True,True,False]
relSubCatsPOHelper CatPar = [False,False,True,False]
relSubCatsPOHelper CatRel = [False,False,False,False]
relSubCatsPO :: RelSubCats -> RelSubCats -> Bool
relSubCatsPO x y = and [x0 <= y0 | (x0,y0) <- zip (relSubCatsPOHelper x) (relSubCatsPOHelper y)]

isCatBijBottom = and $ map (\x -> relSubCatsPO x CatBij) allRelSubCats

relSubCatsExample = pad 1.1 $ withConnections2 (PS.Poset (allRelSubCats,relSubCatsPO)) (CatBij)

--example = pad 1.1 $ withConnections2 (PS.posetD 24) 1
--example = pad 1.1 $ withConnections2 (PS.posetB 5) []
--example = relSubCatsExample
example = pad 1.1 $ withConnections2 (partitionPoset 4) ([[1..4]])

main = mainWith (example :: Diagram B)