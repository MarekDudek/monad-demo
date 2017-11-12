module MechanizedAssemblyLineSpec where


import MechanizedAssemblyLine
import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = 
    describe "assembly line example" $ do
        it "with do notation" $ do
            let t = assemblyLine "wood"
                Contains w = t
            w `shouldBe` Wrapped "wood, roughly chopped, polished, wrapped"
        it "with bind" $ do
            let t = assemblyLine' "wood"
                Contains w = t
            w `shouldBe` Wrapped "wood, roughly chopped, polished, wrapped"
