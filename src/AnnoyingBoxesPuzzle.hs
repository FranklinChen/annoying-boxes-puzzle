-- | Annoying boxes puzzle from a
-- <http://blog.plover.com/math/logic/annoying-boxes.html blog post>.
--
--  There are two boxes on a table, one red and one green. One
--  contains a treasure. The red box is labelled "exactly one of the
--  labels is true". The green box is labelled "the treasure is in
--  this box."
--
-- Can you figure out which box contains the treasure?
module Main where

import Data.Foldable (traverse_)

-- For GHC version < 7.10.1
import Control.Applicative ((<$>), (<*>))

-- | OO-style reverse application operator.
-- Copied from https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Function.html for GHC version < 7.10.1, else "import Data.Function" works.
infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

-- | All information about a box.
data Box = Box { containsTreasure :: Bool
               , labelIsTrue :: Bool
               , labelIsTruthful :: Bool
               }
           deriving (Show)

-- | The state of the world.
data World = World { redBox :: Box
                   , greenBox :: Box
                   }
             deriving (Show)

-- | Constraint to apply when filtering possible worlds.
type Constraint = World -> Bool

-- | For each scenario with a given constraint,
--
-- * Enumerate all possible worlds.
-- * Filter for only the consistent ones.
main :: IO ()
main = do
  putStrLn "Possible consistent worlds for original problem:"
  defaultConstraint & printAllSolutions

  putStrLn "Possible consistent worlds if we assume the red label is truthful:"
  defaultConstraint `with` redLabelIsTruthful & printAllSolutions

  putStrLn "Possible consistent worlds if we assume the green label is truthful:"
  defaultConstraint `with` greenLabelIsTruthful & printAllSolutions

  putStrLn "Possible consistent worlds if we assume both labels are truthful:"
  defaultConstraint `with` bothLabelsAreTruthful & printAllSolutions

-- | Find all solutions and print them to stdout.
printAllSolutions :: Constraint -> IO ()
printAllSolutions constraint =
  anyWorld & filter constraint & traverse_ print

-- | The problem as specified.
defaultConstraint :: Constraint
defaultConstraint world =
  let theRedBox = world & redBox
      theGreenBox = world & greenBox
  in and
     [ (theRedBox & containsTreasure) /= (theGreenBox & containsTreasure)
       -- exactly one box contains the treasure
     , (theRedBox & labelIsTrue)
       ==
       (
         (theRedBox & labelIsTrue) /= (theGreenBox & labelIsTrue)
         -- exactly one label is true
       )
       -- what the red box label says
     , (theGreenBox & labelIsTrue)
       ==
       (theGreenBox & containsTreasure)
       -- what the green box label says
     , theRedBox & labelTruthfulImpliesTrue
       -- what being truthful means
     , theGreenBox & labelTruthfulImpliesTrue
       -- what being truthful means
     ]

-- | Combine constraints.
with :: Constraint -> Constraint -> Constraint
c `with` d = \world -> c world && d world

-- | If a label is truthful, then what it says is true.
labelTruthfulImpliesTrue :: Box -> Bool
labelTruthfulImpliesTrue box =
  (box & labelIsTruthful) `implies` (box & labelIsTrue)

-- Boilerplate for generating all possible values for types.

anyWorld :: [World]
anyWorld = World <$> anyBox <*> anyBox

anyBox :: [Box]
anyBox = Box <$> anyBool <*> anyBool <*> anyBool

anyBool :: [Bool]
anyBool = [False, True]

-- | Implication operator.
implies :: Bool -> Bool -> Bool
p `implies` q = not p || q

-- Extra constraints.

redLabelIsTruthful :: Constraint
redLabelIsTruthful world = world & redBox & labelIsTruthful

greenLabelIsTruthful :: Constraint
greenLabelIsTruthful world = world & greenBox & labelIsTruthful

bothLabelsAreTruthful :: Constraint
bothLabelsAreTruthful = redLabelIsTruthful `with` greenLabelIsTruthful
