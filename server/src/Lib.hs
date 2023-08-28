module Lib (findWordAndBoundaries) where

import qualified Data.Text as T
import           Data.Char (isAlpha)

findWordAndBoundaries :: T.Text -> Int -> Int -> Maybe (T.Text, Int, Int)
findWordAndBoundaries contents line col
  | isAlpha c = Just
    ( leftFragment <> rightFragment
    , col - T.length leftFragment
    , col + T.length rightFragment)
  | otherwise = Nothing
  where
    c = T.index r 0

    (l, r) = T.splitAt col lineContents

    leftFragment = T.reverse . T.takeWhile isAlpha . T.reverse $ l

    rightFragment = T.takeWhile isAlpha r

    lineContents = T.lines contents !! line