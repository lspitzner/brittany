spec = do
  it "creates a snapshot at the given level" . withGraph runDB $ do
    lift $ do
      studentDiagnosticReadingLevel updatedStudent `shouldBe` Just 10 -- x
      elaSnapshotReadingLevel snapshot `shouldBe` 12
