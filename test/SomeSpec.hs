module SomeSpec where


import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = 
    describe "" $ do
        it "" $ do
            pending
