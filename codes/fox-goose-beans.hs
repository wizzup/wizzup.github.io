-- Fox, goose and bag of beans puzzle
-- https://en.wikipedia.org/wiki/Fox,_goose_and_bag_of_beans_puzzle
--
import Data.Set (Set, empty, fromList, toList, isSubsetOf, insert, delete)

data ItemType = Fox
              | Goose
              | Beans
              | Human
  deriving (Eq, Ord)

instance Show ItemType where
  show Fox   = "🐺"
  show Goose = "🐦"
  show Beans = "👝"
  show Human = "🚹"

-- use Set for collection of items since ordering is not important
type Items = Set ItemType

data BoatLoc = Start
             | Finish
  deriving (Show, Eq)

data State = State {
  start :: Items,      -- items at start position
  boatLoc :: BoatLoc,  -- boat location (start or finish)
  finish :: Items      -- item at finish position
  } -- deriving (Show)

instance Show State where
  show s = concatMap show (start s)
        <> "----"
        <> concatMap show (finish s)
  -- show s | boatLoc s == Start = "S"
  --        | otherwise          = "F"

initState :: State
initState = State {
  start = fromList [Fox, Goose, Beans, Human],
  boatLoc = Start,
  finish = empty
}

finalState :: State
finalState = State {
  start = empty,
  boatLoc = Finish,
  finish = fromList [Fox, Goose, Beans, Human]
}

-- all posible next states from input state
possibles :: State -> [State]
possibles s | boatLoc s == Start = f crossSF start
            | otherwise          = f crossFS finish
  where f c p = flip c <$> [s] <*> toList (p s)

crossSF :: ItemType -> State -> State
crossSF i s = State {
  start = delete Human $ delete i $ start s,
  boatLoc = Finish,
  finish = insert Human $ insert i $ finish s
}

crossFS :: ItemType -> State -> State
crossFS i s = State {
  start = insert Human $ insert i $ start s,
  boatLoc = Start,
  finish = delete Human $ delete i $ finish s
}

isBadState :: State -> Bool
isBadState i | start i  == start initState = False
isBadState i | finish i == finish finalState = False

isBadState i = or ( (fg `isSubsetOf`) <$> [s,f])
            || or ( (gb `isSubsetOf`) <$> [s,f])
  where fg = fromList [Fox,Goose]
        gb = fromList [Goose,Beans]
        s = start i
        f = finish i

isGoodState :: State -> Bool
isGoodState = not . isBadState

next :: State -> IO State
next s = do
  print s
  let n = head $ filter isGoodState $ possibles s
  pure n

loop :: State -> IO State
loop s = do
  print s
  n <- next s
  loop n

main :: IO ()
main = do
  loop initState
  pure ()
