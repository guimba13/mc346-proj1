import qualified Data.Map.Strict as M
import Data.List as L
import Data.Maybe

type Node = Char
type Edge = ([Node], Float)
type Graph = ([Node], [Edge])
type Point = (Char, [Float])

type Vertice = [Char] 
type Vizinhos = M.Map Vertice [(Vertice, Float)]

data Md = Md { dist :: M.Map Vertice (Float, Vertice)
             , q :: [Vertice]
             } deriving (Show)

initmydics :: [Vertice] -> Vertice -> Md

initmydics vs init = let
                  q = vs
                  dist = M.insert init (0,init) $ M.fromList $ zip vs $ zip (repeat (1/0)) vs
                  in Md dist q


--minvalkey :: (Ord k, Ord Vertice) => [Vertice] -> M.Map Vertice (Float, Vertice) -> Vertice
minvalkey q dic =  let
            aa = [(v,x) | (x,(v,_)) <- M.toList dic, x `elem` q, v<(1/0)]
            in if null aa then Nothing else Just (snd $ L.minimum aa)

dij :: Vizinhos -> Vertice -> Md -> Md
dij vizinhos final d = let
                    atual = minvalkey (q d) (dist d)
                    in if isNothing atual then (Md M.empty []) else if (fromJust atual)==final then d else dij' vizinhos final (fromJust atual) d


dij' vizinhos final atual d = dij vizinhos final newd
             where
             newq = filter (/=atual) $ q d
             viz = vizinhos M.! atual
             custo = fst $ dist d M.! atual
             
             combine oldd@(Md dist q) (v,step) = if custo+step< fst (dist M.! v)
                                                     then (Md  (M.insert v ((custo+step),atual) dist) newq)
                                                     else oldd
                                                     
             newd = L.foldl' combine (Md (dist d) newq) viz


myproc lines = let
               d = M.empty
               addx a b v  d = if a `M.member` d then  M.insertWith (++) a [(b,v)] d else  M.insert a [(b,v)] d 
               proc' [a,b] d = d
               proc' (l:ls) d = proc' ls $ addx a b (read v::Float)  d
                   where [a,b,t,v] = words l
               in proc' lines d


getLines :: IO [String]
getLines = lines <$> getContents

main = do 
    lines <- getLines
    let
      leadingLines = takeWhile (not . null) lines
      (_:as) = dropWhile (not . null) lines
      middleLines = takeWhile (not . null) as
      (_:path:_) = dropWhile (not . null) as
      (start:end:_) = words path
      vizinhos = myproc leadingLines
      vs = M.keys vizinhos
      d = dij vizinhos end $  initmydics vs start
    printout start end d


printout i f d = let
           path i f d                 
                | i==f = []
                | otherwise = let
                     prev = snd (dist d M.! f)
                     in prev : path i prev d
           in do
              putStrLn ("inicial: " ++ i)
              putStrLn ("final: " ++  f)    
              --print d
              if M.null (dist d) then do
                           putStrLn "nada"
                           else do
                           putStrLn ("custo: "++ (show  (fst  (dist d M.! f))))
                           mapM_ (\ x -> putStr (x ++ " "))  $ L.reverse $ f: path i f d
                           putStrLn ""