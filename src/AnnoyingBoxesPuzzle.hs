-- | Annoying boxes puzzle.
-- http://blog.plover.com/math/logic/annoying-boxes.html
--
--  There are two boxes on a table, one red and one green. One
--  contains a treasure. The red box is labelled "exactly one of the
--  labels is true". The green box is labelled "the treasure is in
--  this box."
--
-- Can you figure out which box contains the treasure?

-- All information about a box.
data Box = Box { _containsTreasure :: Bool
               , _labelIsTrue :: Bool
               , _labelIsTruthful :: Bool
               }
           deriving (Show)

-- | A state of the world.
data World = World { _redBox :: Box
                   , _greenBox :: Box
                   }
             deriving (Show)

-- | Return list of all possible consistent worlds by brute force:
--
-- Enumerate all possibilities.
-- Filter for consistency.
possibleWorlds :: [World]
possibleWorlds =
  [ world
  | world <- anyWorld
  , let redBox = _redBox world
  , let greenBox = _greenBox world

  , _containsTreasure redBox |-| _containsTreasure greenBox

  , _labelIsTrue redBox == _labelIsTrue redBox |-| _labelIsTrue greenBox
  , _labelIsTrue greenBox ==> _containsTreasure greenBox

  , labelTruthfulness redBox
  , labelTruthfulness greenBox
  ]

-- | If a label is truthful, then what it says is true.
labelTruthfulness :: Box -> Bool
labelTruthfulness box = _labelIsTruthful box ==> _labelIsTrue box

anyWorld :: [World]
anyWorld = World <$> anyBox <*> anyBox

anyBox :: [Box]
anyBox = Box <$> anyBool <*> anyBool <*> anyBool

anyBool :: [Bool]
anyBool = [False, True]

-- | Implication operator.
(==>) :: Bool -> Bool -> Bool
p ==> q = not p || q

-- | Exclusive-or operator.
(|-|) :: Bool -> Bool -> Bool
p |-| q = (p || q) && not (p && q)

main :: IO ()
main = do
  putStrLn "Possible consistent worlds:\n"
  mapM_ print possibleWorlds
