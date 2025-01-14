operatorSpec :: Spec
operatorSpec = do
  describe "(&>)" $ do
    it "updates parser input" $ do
      let p1 = do
            _ <- string "old"
            leftover <- getInput
            return leftover
      let p2 = string "new"
      run (p1 &> p2) "oldnew" `shouldBe` Right "new"
      (run (p1 &> p2) "oldx" :: Either (ParseErrorBundle String Void) String)
        `shouldSatisfy` isLeft

  describe "(!?)" $ do
    it "returns the element at index or Nothing" $ do
      let xs = [1 :: Int, 2, 3]
      xs !? 0 `shouldBe` Just 1
      xs !? 2 `shouldBe` Just 3
      xs !? 3 `shouldBe` Nothing
      xs !? (-1) `shouldBe` Nothing