-- | Annoying boxes puzzle.
-- http://blog.plover.com/math/logic/annoying-boxes.html
--
--  There are two boxes on a table, one red and one green. One
--  contains a treasure. The red box is labelled "exactly one of the
--  labels is true". The green box is labelled "the treasure is in
--  this box."
--
-- Can you figure out which box contains the treasure?

import Control.Applicative ((<$>), (<*>))

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
possibleWorlds :: (World -> Bool) -> [World]
possibleWorlds extraConstraint =
  [ world
  | world <- anyWorld
  , let redBox = _redBox world
  , let greenBox = _greenBox world

  , _containsTreasure redBox |-| _containsTreasure greenBox

  , _labelIsTrue redBox == _labelIsTrue redBox |-| _labelIsTrue greenBox
  , _labelIsTrue greenBox ==> _containsTreasure greenBox

  , labelTruthfulness redBox
  , labelTruthfulness greenBox
  , extraConstraint world
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

redLabelIsTruthful :: World -> Bool
redLabelIsTruthful world = _labelIsTruthful (_redBox world)

greenLabelIsTruthful :: World -> Bool
greenLabelIsTruthful world = _labelIsTruthful (_greenBox world)

bothLabelsAreTruthful :: World -> Bool
bothLabelsAreTruthful world =
  redLabelIsTruthful world && greenLabelIsTruthful world

main :: IO ()
main = do
  putStrLn "Possible consistent worlds for original problem:\n"
  mapM_ print $ possibleWorlds (const True)

  putStrLn ""

  putStrLn "Possible consistent worlds if we assume the red label is truthful"
  mapM_ print $ possibleWorlds redLabelIsTruthful

  putStrLn ""

  putStrLn "Possible consistent worlds if we assume the green label is truthful"
  mapM_ print $ possibleWorlds greenLabelIsTruthful

  putStrLn ""

  putStrLn "Possible consistent worlds if we assume both labels are truthful"
  mapM_ print $ possibleWorlds bothLabelsAreTruthful
