{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Aquarium where 

import           Data.Hashable                  (Hashable)
import           Data.Holmes
import qualified Data.JoinSemilattice.Intersect as I
import           Data.Propagator
import           GHC.Generics                   (Generic)
import           Puzzle
import Test.Hspec
--import Symantic.Propagator (Pro)
--import qualified Symantic.Propagator as Pro
import Debug.Trace

data AquariumCell = Air | Water
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

example1 =
  Puzzle
    { _size = 6
    , _colSums = [1, 5, 4, 5, 4, 4]
    , _rowSums = [2, 3, 4, 4, 5, 5]
    , _puzzleRows = [[2, 2, 1, 1], [1, 3, 1, 1], [1, 1, 1, 1, 1, 1], [2, 1, 1, 1, 1], [1, 2, 1, 1, 1], [1, 1, 4]]
    , _puzzleCols = [[1, 5], [1, 2, 1, 2], [2, 3, 1], [3, 3], [5, 1], [6]]
    }

example3 =
  Puzzle
    { _size = 15
    , _colSums = [8, 8, 9, 6, 7, 10, 10, 9, 9, 6, 7, 10, 10, 6, 3]
    , _rowSums = [7, 9, 5, 10, 4, 5, 9, 12, 13, 1, 9, 4, 4, 12, 14]
    , _puzzleRows =
        [ [3, 2, 2, 1, 2, 1, 3, 1]
        , [1, 2, 1, 1, 2, 1, 1, 2, 2, 2]
        , [2, 1, 3, 1, 3, 2, 3]
        , [1, 2, 1, 2, 2, 2, 1, 1, 3]
        , [2, 1, 2, 1, 4, 2, 1, 1, 1]
        , [2, 1, 1, 1, 1, 2, 4, 1, 2]
        , [1, 1, 2, 3, 3, 2, 1, 2]
        , [1, 1, 1, 1, 2, 1, 2, 1, 1, 2, 1, 1]
        , [2, 2, 1, 2, 2, 1, 3, 1, 1]
        , [2, 1, 5, 4, 3]
        , [2, 1, 1, 1, 2, 6, 1, 1]
        , [2, 1, 2, 3, 2, 4, 1]
        , [1, 3, 2, 1, 2, 1, 1, 1, 3]
        , [1, 2, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1]
        , [1, 3, 5, 3, 1, 1, 1]
        ]
    , _puzzleCols =
        [ [1, 5, 2, 1, 6]
        , [2, 1, 1, 2, 3, 3, 2, 1]
        , [4, 2, 2, 2, 2, 2, 1]
        , [1, 4, 2, 2, 2, 2, 2]
        , [2, 1, 1, 2, 1, 3, 2, 2, 1]
        , [1, 1, 1, 4, 1, 1, 1, 1, 1, 1, 2]
        , [1, 2, 3, 2, 1, 1, 1, 1, 2, 1]
        , [2, 1, 4, 2, 1, 1, 1, 1, 2]
        , [2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1]
        , [1, 2, 1, 1, 1, 3, 1, 1, 3, 1]
        , [2, 2, 4, 1, 1, 1, 1, 3]
        , [2, 1, 2, 2, 2, 1, 1, 3, 1]
        , [2, 2, 3, 2, 1, 1, 1, 1, 2]
        , [1, 3, 3, 2, 1, 2, 3]
        , [5, 2, 6, 2]
        ]
    }

example4 = Puzzle {
    _size = 10
  , _colSums = [4, 3, 8, 9, 8, 6, 6, 8, 7, 2]
  , _rowSums = [7, 6, 8, 8, 3, 4, 7, 5, 4, 9]
  , _puzzleRows = [
    [1, 5, 1, 2, 1],
    [1, 1, 1, 2, 1, 2, 1, 1],
    [2, 1, 2, 1, 3, 1],
    [2, 3, 2, 2, 1],
    [1, 1, 1, 2, 2, 2, 1],
    [1, 1, 3, 1, 3, 1],
    [1, 1, 1, 1, 2, 1, 2, 1],
    [1, 1, 2, 3, 2, 1],
    [1, 1, 1, 3, 1, 1, 1, 1],
    [3, 2, 1, 1, 1, 1, 1]
  ]
  , _puzzleCols = [
    [2, 2, 2, 3, 1]
    , [1, 4, 4, 1]
    , [2, 3, 1, 2, 2]
    , [1, 2, 1, 1, 2, 1, 1, 1]
    , [1, 2, 1, 1, 1, 1, 2, 1]
    , [1, 2, 1, 1, 2, 3]
    , [2, 1, 1, 1, 2, 1, 2]
    , [1, 1, 1, 1, 2, 1, 1, 2]
    , [1, 1, 1, 1, 2, 1, 1, 2]
    , [4, 2, 2, 2]
  ]
}

spec_aquarium :: Spec
spec_aquarium = do
  --it "computes Aquarium/exemple1" do
  --  res <- solve example1
  --  (res `shouldBe`) $ Just $ I.fromList <$>
  --    [[Water],[Water],[Air],[Air],[Air],[Air],[Air],[Water],[Water],[Water],[Air],[Air],[Air],[Water],[Air],[Water],[Water],[Water],[Air],[Air],[Water],[Water],[Water],[Water],[Air],[Water],[Water],[Water],[Water],[Water],[Air],[Water],[Water],[Water],[Water],[Water]]
  it "computes Aquarium/example4" do
    res <- solve example4
    (res `shouldBe`) $ Just $ I.fromList <$>
      [ [Air],[Water],[Water],[Water],[Water],[Water],[Air],[Water],[Water],[Air],[Air],[Air],[Water],[Water],[Water],[Air],[Water],[Water],[Water],[Air],[Air],[Air],[Water],[Water],[Water],[Water ],[Water],[Water],[Water],[Water],[Air],[Air],[Water],[Water],[Water],[Water],[Water],[Water],[Water],[Water],[Air],[Air],[Water],[Water],[Water],[Air],[Air],[Air],[Air],[Air],[Air],[Air],[Water],[Water],[Water], [Water],[Air],[Air],[Air],[Air],[Water],[Air],[Air],[Water],[Water],[Water],[Water],[Water],[Water],[Air],[Water],[Air],[Water],[Water],[Air],[Air],[Air],[Water],[Water],[Air],[Water],[Water],[Air],[Air],[Air],[Air],[Water],[Water],[Air],[Air],[Water],[Water],[Water],[Water],[Water],[Water],[Water],[Water],[Water],[Air] ]

solve :: Puzzle -> IO (Maybe [Intersect AquariumCell])
solve p = config p `satisfying` constraints p

config :: Puzzle -> Config Holmes (Intersect AquariumCell)
config p = shuffle $ gridSize `from` [Air, Water]
  where gridSize = _size p * _size p

constraints :: forall m. MonadCell m => Puzzle -> [Prop m (Intersect AquariumCell)] -> Prop m (Intersect Bool)
constraints p board =
  let rows, cols :: [[Prop m (Intersect AquariumCell)]]
      rows = calculateRows p board
      cols = calculateCols p board

      groups :: [[(Prop m (Intersect AquariumCell), Int)]]
      groups = groupsWithDepth p board

      water :: Prop m (Intersect AquariumCell)
      water = lift Water

   in and' $ concat [
      zipWith (\row totWater -> exactly totWater (water .==) row) rows (_rowSums p),
      zipWith (\col totWater -> exactly totWater (water .==) col) cols (_colSums p),
      map noAirBubbles groups
    ]

noAirBubbles
  :: forall m . MonadCell m
  => [(Prop m (Intersect AquariumCell), Int)]
  -> Prop m (Intersect Bool)
noAirBubbles gs =
  flip allWithIndex' gs $ \i (cell, depth) ->
    flip allWithIndex' gs $ \i2 (cell2, depth2) ->
      if i2 /= i && depth >= depth2
        then or' [
            cell2 .== lift Air,
            cell .== lift Water
          ]
        else
          (true :: Prop m (Intersect Bool))
