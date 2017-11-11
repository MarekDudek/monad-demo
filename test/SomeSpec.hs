module SomeSpec where


import MechanizedAssemblyLine
import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = 
    describe "assembly line example" $ do
        it "works" $ do
            let w = "some wood" 
                wr = assemblyLine w 
            putStrLn (show wr)
            pending
