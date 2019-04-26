import Test.Hspec

import qualified Maze.Generate.Worm.Spec


main :: IO ()
main = hspec spec

spec = do
  describe "Maze.Generate.Worm.Spec" Maze.Generate.Worm.Spec.spec
