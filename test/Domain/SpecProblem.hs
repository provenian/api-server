module Domain.SpecProblem where

import Control.Monad.Reader
import Data.Reflection
import Test.Tasty.Hspec

import Domain.App
import qualified Domain.Model.Problem.Problem as Problem
import qualified Domain.Model.Problem.CreateInput as CreateInput
import qualified Domain.Service.Problem as Problem
import qualified Infra.Repository.ProblemRepoStub

spec_Service :: Spec
spec_Service = return ()

{-
  let service =
        give Infra.Repository.ProblemRepoStub.new $ Problem.ProblemService

  describe "service" $ do
    it "should list" $ do
      ps <- flip runReaderT AppState{} $ Problem.list service
      ps `shouldNotBe` []
    it "should create" $ do
      ps <- flip runReaderT AppState{}
        $ Problem.create service (CreateInput.CreateInput{})
      ps `shouldBe` ()
-}
