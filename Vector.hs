module Vector (module Data.Complex, module Vector) where

import Data.Complex

type Vector = Complex Float
j, zero :: Vector
j = 0:+1
zero = 0:+0
normalize a = a / (sqrt $ a * conjugate a)