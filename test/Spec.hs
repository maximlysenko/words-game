import Test.Hspec
import Grid
import Data

gwc :: Grid Cell
gwc = gridWithCoords grid

testFindWord :: String -> Expectation
testFindWord word =
    let (Just result) = findWord gwc word
        string = map cell2Char result
    in string `shouldBe` word

main :: IO ()
main = hspec $ do
    describe "findWord" $ do
        it "Should find words that exist on a Grid" $ do
            testFindWord "HASKELL"
            testFindWord "PERL"
        it "Should not find words that do not exist on a Grid" $ do
            (findWord gwc "JAVASCRIPT") `shouldBe` Nothing
