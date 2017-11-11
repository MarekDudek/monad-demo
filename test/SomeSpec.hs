module SomeSpec where


import MechanizedAssemblyLine
import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = 
    describe "assembly line example" $ do
        it "works" $ do
            let t = assemblyLine "wood"
                Contains w = t
            w `shouldBe` Wrapped "wood, roughly chopped, polished, wrapped"
