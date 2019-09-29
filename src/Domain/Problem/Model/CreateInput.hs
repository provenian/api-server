module Domain.Problem.Model.CreateInput where

import Domain.Problem.Model.Problem (Problem(Problem))
import qualified Data.Text as T

data CreateInput = CreateInput {
  title :: T.Text,
  contentType :: T.Text,
  content :: T.Text,
  createdAt :: Int,
  updatedAt :: Int,
  writer :: T.Text,
  files :: [T.Text],
  languages :: [T.Text],
  tags :: [T.Text]
}

fromCreateInput :: CreateInput -> T.Text -> Problem
fromCreateInput i key = Problem key
                                (title i)
                                (contentType i)
                                (content i)
                                (createdAt i)
                                (updatedAt i)
                                (writer i)
                                (files i)
                                (languages i)
                                (tags i)
