-- file: Cuboid.hs
-- author: Jacob Xie
-- date: 2023/07/15 13:10:27 Saturday
-- brief:

module Geometry.Cuboid
  ( volume,
    area,
  )
where

volume :: Float -> Float -> Float -> Float
volume = ((*) .) . rectangleArea

area :: Float -> Float -> Float -> Float
area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea = (*)
