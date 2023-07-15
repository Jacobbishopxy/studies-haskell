-- file: Sphere.hs
-- author: Jacob Xie
-- date: 2023/07/15 13:10:17 Saturday
-- brief:

module Geometry.Sphere
  ( volume,
    area,
  )
where

volume :: Float -> Float
volume = (* (4.0 / 3.0 * pi)) . (^ 3)

area :: Float -> Float
area = (* (4 * pi)) . (^ 3)
