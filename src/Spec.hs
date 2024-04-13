module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Utilización de mayorSegun/3" $ do
    it "El mayor entre el valor absoluto de -5 y 2 es -5 " $ do
      mayorSegun abs (-5) 2 `shouldBe` -5

  describe "Utilización de maximoSegun/2" $ do
    it "El maximo de la lista de valor absoluto es -10" $ do
      maximoSegun abs [1,2,3,4,5,6,-10] `shouldBe` -10
    it "El maximo de la lista de un element es dicho elememnto" $ do
      maximoSegun abs [4] `shouldBe` 4

  describe "Funcion sinRepetidos/1" $ do
    it "La lista [1,2,3,4,1] aplicando sinRepetidos es igual a [1,2,3,4]" $ do
      sinRepetidos [1,2,3,4,1] `shouldBe` [1,2,3,4]
