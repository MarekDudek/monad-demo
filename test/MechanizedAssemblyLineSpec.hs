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
        it "with do notation, 2" $ do
            let t = assemblyLine2  "wo"
            t `shouldBe` Contains2 (Wrapped "wo, roughly chopped, polished, wrapped")
        it "with bind, 2" $ do
            let t = assemblyLine2' "wo"
            t `shouldBe` Contains2 (Wrapped "wo, roughly chopped, polished, wrapped")
        it "2, failure in 'make'   function" $ do
            let t = assemblyLine2' ""
            t `shouldBe` Failed2 "wood was empty"
        it "2, failure in 'polish' function" $ do
            let t = assemblyLine2' "1"
            t `shouldBe` Failed2 "number of chopstics was divisible by three" 
        it "2, failure in 'wrap'   function" $ do
            let t = assemblyLine2' "12345"
            t `shouldBe` Failed2 "number of chopstics was divisible by four"
