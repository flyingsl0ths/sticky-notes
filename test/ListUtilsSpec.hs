import ListUtils (indexOf)
import Test.Hspec
  ( Spec,
    describe,
    hspec,
    it,
    shouldBe,
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ListUtils.indexOf" $ do
  it "returns -1 when the list is empty" $ do
    let index = indexOf 'a' []
    index `shouldBe` -1

  it "returns -1 when the list contains a single non-matching item" $ do
    let index = indexOf 'a' ['b']
    index `shouldBe` -1

  it "returns 0 when the list contains a single matching item" $ do
    let index = indexOf 'a' ['a']
    index `shouldBe` 0

  it "returns the length of the list when the item is not found" $ do
    let word = "food"
    let index = indexOf 'a' word
    index `shouldBe` length word

  it "returns the index of the item in the list when found" $ do
    let word = "foo.db"
    let toFind = '.'
    let index' = indexOf toFind word
    (word !! index') `shouldBe` toFind
