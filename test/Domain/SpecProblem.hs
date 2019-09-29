module Domain.SpecProblem where

import Control.Monad.Reader
import Data.Reflection
import Test.Tasty.Hspec

import Domain.App
import qualified Domain.Problem as Problem
import qualified Infra.Repository.ProblemRepoStub

spec_Service :: Spec
spec_Service = do
  let service =
        give Infra.Repository.ProblemRepoStub.new $ Problem.ProblemService

  describe "service" $ do
    it "should list" $ do
      ps <- flip runReaderT AppState{} $ Problem.list service
      ps `shouldNotBe` []
    it "should create" $ do
      ps <- flip runReaderT AppState{}
        $ Problem.create service (Problem.CreateInput{})
      ps `shouldBe` ()
