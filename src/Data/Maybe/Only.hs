module Data.Maybe.Only where

mayOnly :: [a] -> Maybe a
mayOnly [] = Nothing
mayOnly [a] = Just a
