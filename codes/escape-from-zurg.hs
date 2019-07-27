{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MonoLocalBinds #-}

import Data.List

type Space s m = [([m],s)]

class SearchProblem s m where
  trans :: s -> [(m,s)]
  isSolution :: ([m],s) -> Bool

  space, solutions :: s -> Space s m
  space s = step ++ expand step
    where
      step = [([m],t) | (m,t) <- trans s ]
      expand ss = [ (ms ++ ns, t) |
                    (ms,s') <- ss,
                    (ns,t) <- space s']

  solutions = filter isSolution . space

data Toy = Buzz
         | Hamm
         | Rex
         | Woody
  deriving (Eq,Ord,Show)

data Pos = L
         | R
  deriving (Eq,Show)

type Group = [Toy]

type BridgePos = (Pos,Group)

type Move = Either Toy Group

-- printMove :: Move -> IO ()
-- printMove (Left toy) = print toy
-- printMove (Right group) = print group

toys :: [Toy]
toys = [Buzz,Hamm,Rex,Woody]

time :: Toy -> Int
time Buzz  = 5
time Woody = 10
time Rex   = 20
time Hamm  = 25

totalDuration :: [Move] -> Int
totalDuration = sum . durations

durations :: [Move] -> [Int]
durations = map moveDuration

moveDuration :: Move -> Int
moveDuration = either time (maximum . map time)

backw :: Group -> [(Move, BridgePos)]
backw xs = [(Left x, (L, sort (x:(toys \\ xs)))) | x <- xs]

forw :: Group -> [(Move, BridgePos)]
forw xs = [ (Right [x,y], (R, delete y ys))
          | x <- xs, let ys = delete x xs, y <- ys, x < y]

instance SearchProblem BridgePos Move where
  trans (L,l) = forw l
  trans (R,l) = backw (toys \\ l)
  isSolution (ms,s) = s == (R, [])
                   && totalDuration ms <= 60

solution :: SearchProblem BridgePos Move
         => Space BridgePos Move
solution = solutions (L,toys)

main :: IO ()
main = mapM_ print solution
