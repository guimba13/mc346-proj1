import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe
-- vizinho = map (vertice, [(vizinho,dist)])

--  aux - map (vetice distancia)


type Vertice = [Char] 
type Vizinhos = M.Map Vertice [(Vertice, Float)]

data Md = Md { dist :: M.Map Vertice (Float, Vertice)
             , q :: [Vertice]
             } deriving (Show)

initmydics :: [Vertice] -> Vertice -> Md

initmydics vs init = let
                  q = vs
                  dist = M.insert init (0,init) $ M.fromList $zip vs $ zip (repeat (1/0)) vs
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



           

main = do
     -- vizinhos = M.fromList [("a",[("b",10.0),("c",11.0),("d",150.0)]),
     --                        ("b",[("a",12.3),("c",4.0),("d",10)]),
     --                        ("c",[("b",4.0),("d",1.0)]),
     --                        ("d",[("a",4.1),("b",4.0),("c",2)])
     --                        ]
     lines <-getContents
     let 
       (initial,final,vizinhos) = proc lines 
       vs = M.keys vizinhos
       d = dij vizinhos final $  initmydics vs initial
     --print d
     printout initial final d


proc lines = let
               ls = words lines
               d = M.empty
               addx a b v  d = if a `M.member` d then  M.insertWith (++) a [(b,v)] d else  M.insert a [(b,v)] d 
               proc' [a,b] d = (a,b,d)
               proc' (a:b:v:ls) d = proc' ls $ addx a b (read v::Float)  d
               in proc' ls d

--printout :: Vertice -> Vertice -> Md -> IO ()
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