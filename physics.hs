import Data.Complex


data Vec a = Vec a a
main = do print "funguje"

data Foo2 = Bar2 | Baz2 {bazNumber::Int, bazName::String}

h :: Foo2 -> Int
h Baz2 {bazName=name} = length name
h Bar2 {} = 0

a :: Complex Float
a = 1 :+ 0

type Vector = Complex Float
b :: Vector
b = 1 :+ 0
