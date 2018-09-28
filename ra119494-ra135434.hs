import qualified Data.Map.Strict as M
import Data.List as L

type Node = Char
type Edge = ([Node], Float)
type Graph = ([Node], [Edge])
type Point = (Char, [Float])

proc lines = let
               ls = words lines
               d = M.empty
               addx a b v  d = if a `M.member` d then  M.insertWith (++) a [(b,v)] d else  M.insert a [(b,v)] d 
               proc' [a,b] d = (a,b,d)
               proc' (a:b:t:v:ls) d = proc' ls $ addx a b (read v::Float)  d
               in proc' ls d


myproc lines = let
               d = M.empty
               addx a b v  d = if a `M.member` d then  M.insertWith (++) a [(b,v)] d else  M.insert a [(b,v)] d 
               proc' [a,b] d = (a,b,d)
               proc' (l:ls) d = proc' ls $ addx a b (read v::Float)  d
                   where [a,b,t,v] = words l
               in proc' lines d

getLines :: IO [String]
getLines = lines <$> getContents

main = do 
    lines <- getLines
    let
      leadingLines = takeWhile (not . null) lines
      (a:as) = dropWhile (not . null) lines
      middleLines = takeWhile (not . null) as
      (b:path:bs) = dropWhile (not . null) as
      (initial, final, vizinhos) = myproc leadingLines

    print(leadingLines)
    print(middleLines)
    print (path)
    -- print (vizinhos)
    -- print final
    -- print vizinhos