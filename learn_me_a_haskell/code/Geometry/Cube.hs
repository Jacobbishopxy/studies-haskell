-- file: Cube.hs
-- author: Jacob Xie
-- date: 2023/07/15 13:10:43 Saturday
-- brief:

module Geometry.Cube
  ( volume,
    area,
  )
where

import Control.Monad
import Geometry.Cuboid qualified as Cuboid

volume :: Float -> Float
volume = join (join Cuboid.volume)

area :: Float -> Float
area = join (join Cuboid.area)
