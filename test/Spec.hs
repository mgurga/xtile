import Test.Tasty
import StateTests (stateTests)
import OperatorTests (operatorTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [stateTests, operatorTests]
