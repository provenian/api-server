import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Test.Tasty
import Test.Tasty.Hspec

import API

(_:<|>listProblems:<|>_):<|>_ = client api

problemsSpec env = describe "problems" $ do
  it "should list problems" $ do
    res <- runClientM listProblems env
    res `shouldBe` Right []

main = do
  mgr <- newManager defaultManagerSettings
  let env = mkClientEnv mgr (BaseUrl Http "localhost" 1234 "")

  spec <- testSpec "spec" $ problemsSpec env
  defaultMain (testGroup "tests" [spec])
