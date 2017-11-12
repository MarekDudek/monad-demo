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
            t `shouldBe` Failed2 "number of chopsticks was divisible by three" 
        it "2, failure in 'wrap'   function" $ do
            let t = assemblyLine2' "12345"
            t `shouldBe` Failed2 "number of chopsticks was divisible by four"
        it "3, 'make' function" $ do
            makeChopsticks3 "wood" `shouldBe` Contains  "wood, roughly chopped"
            makeChopsticks3 "wood" `shouldBe` Contains2 "wood, roughly chopped"
            makeChopsticks3 "wood" `shouldBe` Just      "wood, roughly chopped"
            makeChopsticks3 "wood" `shouldBe` (Right    "wood, roughly chopped" :: Either String Chopsticks)
        it "3, 'polish' function" $ do
            polishChopsticks3 "wood, roughly chopped" `shouldBe` Contains  "wood, roughly chopped, polished"
            polishChopsticks3 "wood, roughly chopped" `shouldBe` Contains2 "wood, roughly chopped, polished"
            polishChopsticks3 "wood, roughly chopped" `shouldBe` Just      "wood, roughly chopped, polished"
            polishChopsticks3 "wood, roughly chopped" `shouldBe` (Right    "wood, roughly chopped, polished" :: Either String Chopsticks)
        it "3, 'wrap' function" $ do
            wrapChopsticks3 "wood, roughly chopped, polished" `shouldBe` Contains  (Wrapped "wood, roughly chopped, polished, wrapped")
            wrapChopsticks3 "wood, roughly chopped, polished" `shouldBe` Contains2 (Wrapped "wood, roughly chopped, polished, wrapped")
            wrapChopsticks3 "wood, roughly chopped, polished" `shouldBe` Just      (Wrapped "wood, roughly chopped, polished, wrapped")
            wrapChopsticks3 "wood, roughly chopped, polished" `shouldBe` (Right    (Wrapped "wood, roughly chopped, polished, wrapped") :: Either String (Wrapped Chopsticks))
        it "3, assembly line" $ do
            assemblyLine3 "wood" `shouldBe` Contains  (Wrapped "wood, roughly chopped, polished, wrapped")
            assemblyLine3 "wood" `shouldBe` Contains2 (Wrapped "wood, roughly chopped, polished, wrapped")
            assemblyLine3 "wood" `shouldBe` Just      (Wrapped "wood, roughly chopped, polished, wrapped")
            assemblyLine3 "wood" `shouldBe` (Right    (Wrapped "wood, roughly chopped, polished, wrapped") :: Either String (Wrapped Chopsticks))
        it "3, failure in 'make'   function" $ do
            assemblyLine3 "" `shouldBe` Empty 
            assemblyLine3 "" `shouldBe` Failed2 "wood was empty"
            assemblyLine3 "" `shouldBe` Nothing
            -- assemblyLine3 "" `shouldBe` (Left "wood was empty" :: Either String (Wrapped Chopsticks))
