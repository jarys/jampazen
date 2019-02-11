module Physics (module Data.Complex, module Physics) where

import Data.Complex

type Vector = Complex Float
j :: Vector
j = 0:+1

normalize :: RealFloat a => Complex a -> Complex a
normalize (0:+0) = 0:+0
normalize a = a / (sqrt $ a * conjugate a)

toxy :: Complex a -> (a, a)
toxy (x :+ y) = (x, y)

toVector :: Float -> Vector
toVector x = x:+0