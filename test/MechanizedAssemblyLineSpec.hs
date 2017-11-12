module MechanizedAssemblyLineSpec where


import MechanizedAssemblyLine
import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = 
    describe "assembly line example" $ do
        it "with do notation" $ do
            let t = assemblyLine  "wood"
            t `shouldBe` Contains (Wrapped "wood, roughly chopped, polished, wrapped")
        it "with bind" $ do
            let t = assemblyLine' "wood"
            t `shouldBe` Contains (Wrapped "wood, roughly chopped, polished, wrapped")
        it "works in failure case, do version" $ do
            let t = assemblyLine  ""
            t `shouldBe` Empty
        it "works in failure case, bind version" $ do
            let t = assemblyLine' ""
            t `shouldBe` Empty
