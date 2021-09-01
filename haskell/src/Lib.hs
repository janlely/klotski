{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( readAndSolve 
    ) where
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.List as L
import Data.Maybe
import Data.Bifunctor (second)
import Control.Monad
import Debug.Trace (trace)
import qualified Control.Monad.State.Strict as SS
import Data.List.Split (chunksOf)

type BlockName = String
data Direction = L| R| U | D deriving Show
type Move = (BlockName, Situation, Situation) 
type Board = (Int, Int)  
data Block = Block {
    cells :: [Coordinate],
    topLeft :: Coordinate
    } deriving Show
type Coordinate = (Int, Int)
type Situation = A.Array Int String

-- sitToString :: Int -> Situation -> String
-- sitToString width sit = let ls = A.elems sit
                            -- ls1 = chunksOf width ls
                            -- ls2 = unwords <$> ls1
                         -- in L.intercalate "\n" ls2 ++ "\n"


trackResult :: M.Map Situation (BlockName, Situation) -> Situation -> [(BlockName, Situation, Situation)]
trackResult m sit = if M.member sit m
                       then let (name, start) = fromJust $ M.lookup sit m
                             in (name, start, sit):trackResult m start 
                       else []


moveByDirection :: [Coordinate] -> Direction -> [Coordinate]
moveByDirection cos L = (\(x,y) -> (x, y-1)) <$> cos
moveByDirection cos R = (\(x,y) -> (x, y+1)) <$> cos
moveByDirection cos U = (\(x,y) -> (x-1, y)) <$> cos
moveByDirection cos D = (\(x,y) -> (x+1, y)) <$> cos

                          
coorToIdx :: Board -> Int -> Int -> Int
coorToIdx (_, nc) x y = x*nc + y

idxToCoor:: Board -> Int -> (Int, Int)
idxToCoor (_, nc) i = (i `div` nc, i `mod` nc)

inBoard :: Board -> Int -> Int -> Bool
inBoard (nr, nc) x y = x >= 0 && x < nr && y >= 0 && y < nc


klotski :: Board -> [String] -> Situation -> (BlockName, [Int]) -> Maybe [Move]
klotski board names sit (name, dest) = let (m, s) =  bfs (S.singleton sit) [sit] (M.empty, Nothing)
                                        in case s of
                                             Nothing -> Nothing
                                             Just s' -> Just $ trackResult m s'
    where bfs _ [] out = out
          bfs seen (e:es) out@(m,_) = let validMoves = findValidMoves board names e seen
                                          endS = listToMaybe $ filter success $ snd <$> validMoves
                                          newM = L.foldl' (f e) m validMoves
                                          queue =  snd <$> validMoves
                                          seen' =  L.foldl' (flip S.insert) seen queue
                                          queue' = es ++ queue
                                       in if isJust endS
                                             then (newM, endS)
                                             else bfs seen' queue' (newM, Nothing)
          success e = and ((\idx -> e A.! idx == name) <$> dest)
          f e m move@(bn,s) = case M.lookup e m of
                                       Nothing -> M.insert s (bn, e) m
                                       Just _ -> M.insert s (bn, e) m

findValidMoves :: Board -> [String] -> Situation -> S.Set Situation -> [(BlockName, Situation)]
findValidMoves board names sit seen = let movements = concatMap (\name -> (name,) <$> moveOneStep board sit seen name) names
                                       in filter (\(_, s) -> not (S.member s seen)) movements

                          
updateSituation :: Board -> Situation -> BlockName -> [Coordinate] -> [Coordinate] -> Situation
updateSituation board oldSit name oldCos newCos = let newCos' = (\(x,y) -> (coorToIdx board x y, name)) <$> newCos
                                                      oldCos' = (\(x,y) -> (coorToIdx board x y, ".")) <$> oldCos
                                                      combinedCos = L.nubBy (\a b -> fst a == fst b) $ newCos' ++ oldCos'
                                                   in A.accum (\_ x -> x) oldSit combinedCos
    
    
moveOneStep :: Board -> Situation -> S.Set Situation -> BlockName -> [Situation]
moveOneStep board sit initSeen name = bfs initSeen [sit]  []
    where bfs _ [] out = out
          bfs seen (s:ss) out = let pos = ((idxToCoor board) . fst) <$> filter (\(i,e) -> e == name) (A.assocs s)
                                    validMoves = filter (check seen) $ (\d -> let newCells = moveByDirection pos d
                                                                               in (newCells, updateSituation board s name pos newCells)) <$> [L,R,U,D]
                                    queue =  (\(_,x) -> x) <$> validMoves
                                    seen' =  L.foldl' (flip S.insert) seen queue
                                    newOut = out ++ queue
                                    queue' = ss ++ queue
                                 in bfs seen' queue' newOut
          check seen (cos, s)= all (\(x,y) ->  let idx = coorToIdx board x y
                                                in inBoard board x y && (let v = sit A.! idx in v == "." || v == name)) cos && (not $ S.member s seen)


makeBlockMap :: [(Int, Int, String)] -> M.Map BlockName Block
makeBlockMap cos = L.foldl' f M.empty cos
    where f m (x,y,c) = case M.lookup c m of
                          Nothing -> M.insert c (createBlock x y) m
                          Just block -> M.update (Just . (updateBlock x y)) c m

createBlock :: Int -> Int -> Block
createBlock x y = Block {cells = [(x,y)], topLeft = (x,y)}
--
updateBlock :: Int -> Int -> Block -> Block
updateBlock x y block = let oldTop = fst $ topLeft block
                            oldLeft = snd $ topLeft block
                            newTop = if x < oldTop then x else oldTop
                            newLeft = if y < oldLeft then y else oldLeft
                            oldCells = cells block
                         in Block {cells = (x,y):oldCells, topLeft = (newTop, newLeft)}

makeSituation :: Board -> [(Int, Int, String)] -> Situation
makeSituation board@(nr, nc) cos = let rb = nr * nc
                                       arr = A.listArray (0, rb-1) $ replicate rb "."
                                       vs = (\(x,y,c) -> (coorToIdx board x y, c)) <$> cos
                                    in A.accum (\_ a -> a) arr vs

situationToBlockMap :: Board -> Situation -> M.Map BlockName Block
situationToBlockMap board sit = makeBlockMap (f <$> (filter (\(_, c) -> c /= ".") $ A.assocs sit))
    where f (idx, c) = let (i, j) = idxToCoor board idx
                        in (i, j, c)

resultToString :: Board -> [Move] -> [String]
resultToString board ms = f <$> ms
    where f (name, start, end) = let blockStart = fromJust $ M.lookup name $ situationToBlockMap board start
                                     blockEnd = fromJust $ M.lookup name $ situationToBlockMap board end
                                  in name ++ " " ++ show (topLeft blockStart) ++ " " ++ show (topLeft blockEnd)



getDest :: Board -> BlockName -> Situation -> (Int, Int) -> [Int]
getDest board name sit (x, y) = let pos = ((idxToCoor board) . fst) <$> filter (\(i,e) -> e == name) (A.assocs sit)
                                    (xs, ys) = unzip pos
                                    (xt, yt) = (minimum xs, minimum ys)
                                    (xd, yd) = (x - xt, y - yt)
                                    dpos = (\(i, j) -> (i + xd, j + yd)) <$> pos
                                 in (\(i,j) -> coorToIdx board i j) <$> dpos
getNames :: Situation -> [String]
getNames sit = let pos = snd <$> filter (\(i,e) -> e /= ".") (A.assocs sit)
                in L.nub pos


readAndSolve = do
    [nr, nc] <- (fmap (read::String->Int)) . words <$> getLine
    xs <- forM [1..nr] (\i -> do
        line <- words <$> getLine
        return $ zip [0,1..] line)
    c <- getLine
    [i,j] <- ((fmap (read::String->Int)) . words) <$> getLine
    let cos = filter (\(x,ty,c) -> head c /= '.') $ concat $ zipWith (\x row -> (\(y, ch) ->(x, y, ch)) <$> row) [0,1..] xs
        sit = makeSituation (nr,nc) cos
        name = c
        dest = getDest (nr, nc) name sit (i,j)
        names = getNames sit
        result = fromJust $ klotski (nr, nc) names sit (name, dest)
    sequence $ putStrLn <$> resultToString (nr, nc) (reverse result)
    

