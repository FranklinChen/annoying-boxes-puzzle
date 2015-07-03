-- | Annoying boxes puzzle.
-- http://blog.plover.com/math/logic/annoying-boxes.html
--
--  There are two boxes on a table, one red and one green. One
--  contains a treasure. The red box is labelled "exactly one of the
--  labels is true". The green box is labelled "the treasure is in
--  this box."
--
-- Can you figure out which box contains the treasure?

-- Not needed in GHC 7.10 onward, but provided only for
-- compatibility for older GHC versions.
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

-- | For each constraint,
-- generate a list of all possible consistent worlds by brute force:
--
-- Enumerate all possibilities.
-- Filter for consistency.
main :: IO ()
main = do
  putStrLn "Possible consistent worlds for original problem:\n"
  mapM_ print $ filter defaultConstraint
              $ anyWorld

  putStrLn ""

  putStrLn "Possible consistent worlds if we assume the red label is truthful"
  mapM_ print $ filter (defaultConstraint `with` redLabelIsTruthful)
              $ anyWorld

  putStrLn ""

  putStrLn "Possible consistent worlds if we assume the green label is truthful"
  mapM_ print $ filter (defaultConstraint `with` greenLabelIsTruthful)
              $ anyWorld


  putStrLn ""

  putStrLn "Possible consistent worlds if we assume both labels are truthful"
  mapM_ print $ filter (defaultConstraint `with` bothLabelsAreTruthful)
              $ anyWorld


-- | Problem as specified.
defaultConstraint :: World -> Bool
defaultConstraint world =
  let redBox = _redBox world
      greenBox = _greenBox world
  in _containsTreasure redBox `xor` _containsTreasure greenBox
     && _labelIsTrue redBox == _labelIsTrue redBox `xor` _labelIsTrue greenBox
     && _labelIsTrue greenBox `implies` _containsTreasure greenBox
     && labelTruthfulness redBox
     && labelTruthfulness greenBox

-- | Combine constraints
with :: (World -> Bool) -> (World -> Bool) -> (World -> Bool)
x `with` y = \world -> x world && y world

-- | If a label is truthful, then what it says is true.
labelTruthfulness :: Box -> Bool
labelTruthfulness box = _labelIsTruthful box `implies` _labelIsTrue box

anyWorld :: [World]
anyWorld = World <$> anyBox <*> anyBox

anyBox :: [Box]
anyBox = Box <$> anyBool <*> anyBool <*> anyBool

anyBool :: [Bool]
anyBool = [False, True]

-- | Implication operator.
implies :: Bool -> Bool -> Bool
p `implies` q = not p || q

-- | Exclusive-or operator.
xor :: Bool -> Bool -> Bool
p `xor` q = (p || q) && not (p && q)

redLabelIsTruthful :: World -> Bool
redLabelIsTruthful world = _labelIsTruthful (_redBox world)

greenLabelIsTruthful :: World -> Bool
greenLabelIsTruthful world = _labelIsTruthful (_greenBox world)

bothLabelsAreTruthful :: World -> Bool
bothLabelsAreTruthful world =
  redLabelIsTruthful world && greenLabelIsTruthful world
