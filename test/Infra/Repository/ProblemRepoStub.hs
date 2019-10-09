module Infra.Repository.ProblemRepoStub where

import Prelude hiding (id)
import qualified Data.Map as M
import Domain.Model.Problem.Problem
import Domain.Interface.IProblemRepo

data ProblemStub = ProblemStub

new :: SomeProblemRepo
new = SomeProblemRepo ProblemStub

stubData = M.fromList $ map
  (\p -> (id p, p))
  [ Problem
      { id          = "1234"
      , title       = "title A"
      , contentType = "text/markdown"
      , content     = "foo"
      , createdAt   = 1569750022
      , updatedAt   = 1569750022
      , writer      = "nyan"
      , files       = []
      , languages   = ["isabelle"]
      , tags        = ["Hard"]
      }
  ]

instance IProblemRepo ProblemStub where
  getByID _ k = return $ M.lookup k stubData
  create _ _ = return ()
  list _ = return $ M.elems stubData
