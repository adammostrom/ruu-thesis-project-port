module Main where
import Test_SDP


main :: IO()
main = do runTests

runTests :: IO ()
runTests = do
    Test_SDP.testAll