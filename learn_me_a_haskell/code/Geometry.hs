-- file: Geometry.hs
-- author: Jacob Xie
-- date: 2023/07/15 00:47:27 Saturday
-- brief:

module GeometryV1
  ( sphereVolume,
    sphereArea,
    cubeVolume,
    cubeArea,
    cuboidArea,
    cuboidVolume,
  )
where

import Control.Monad

sphereVolume :: Float -> Float
-- sphereVolume radius = (4.0 / 3.0) * pi * (radius ^3)
sphereVolume = (* (4.0 / 3.0 * pi)) . (^ 3)

sphereArea :: Float -> Float
-- sphereArea radius = 4 * pi * (radius ^ 2)
sphereArea = (* (4 * pi)) . (^ 3)

cubeVolume :: Float -> Float
-- cubeVolume side = cuboidVolume side side side
cubeVolume = join (join cuboidVolume)

cubeArea :: Float -> Float
-- cubeArea side = cuboidArea side side side
cubeArea = join (join cuboidArea)

cuboidVolume :: Float -> Float -> Float -> Float
-- cuboidVolume a b c = rectangleArea a b * c
cuboidVolume = ((*) .) . rectangleArea

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
-- rectangleArea a b = a * b
rectangleArea = (*)
