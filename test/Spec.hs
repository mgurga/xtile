import Test.Tasty
import StateTests (stateTests)
import OperatorTests (operatorTests)
import HeuristicsTests (heuristicsTests)
import UtilsTests (utilsTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [stateTests, operatorTests, utilsTests, heuristicsTests]
