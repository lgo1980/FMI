module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de Thanos - Punto 1" $ do
    it "Si el guantalete con 6 gemas y es de uru entonces debe devolver la mitad de personajes del universo" $ do
      2 + 1 `shouldBe` 3
  --   it "Si el guantalete con 6 gemas y no es de uru e +ntonces debe devolver los mismos personajes del universo" $ do
  --     chasquido universoMarvel guantaleteCon6GemasSinUru `shouldBe` personajes universoMarvel
  --   it "Si el guantalete sin 6 gemas y es de uru entonces debe devolver los mismos personajes del universo" $ do
  --     chasquido universoMarvel guantaleteSin6GemasConUru `shouldBe` personajes universoMarvel
  --   it "Si el guantalete sin 6 gemas y no es de uru entonces debe devolver los mismos personajes del universo" $ do
  --     chasquido universoMarvel guantaleteSin6GemasSinUru `shouldBe` personajes universoMarvel
  -- describe "Test de Thanos - Punto 2" $ do
  --   it "Como ironMan tiene menos de 45 años satiface la funcion aptoPendex" $ do
  --     universoMarvel `shouldSatisfy` aptoPendex
  --   it "Como no hay nadie con menos de 45 años no satiface la funcion aptoPendex" $ do
  --     universoMarvelNoPendex `shouldNotSatisfy` aptoPendex 
