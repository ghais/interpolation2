module Numeric.Interpolation.Private.Basis where

import Numeric.Interpolation.Private.Piece (sqr)

import qualified Data.List.Match as Match
import Data.Coerce (Coercible, coerce)


_hermite1Split :: [a] -> [b] -> [(b, b)]
_hermite1Split xs = uncurry zip . Match.splitAt xs

hermite1Split :: [a] -> [b] -> [(b, b)]
hermite1Split _ = pairs

pairs :: [a] -> [(a,a)]
pairs (x0:x1:xs) = (x0,x1) : pairs xs
pairs [] = []
pairs _ = error "pairs: odd number of elements"


parabolaDerivative :: (Fractional a, Coercible a b, Num b) => (a,b) -> (a,b) -> (a,b) -> a -> (b,b)
parabolaDerivative (x0,y0) (x1,y1) (x2,y2) x =
   let l0 = coerce $ (x-x1)*(x-x2)/((x0-x1)*(x0-x2))
       l1 = coerce $ (x-x0)*(x-x2)/((x1-x0)*(x1-x2))
       l2 = coerce $ (x-x0)*(x-x1)/((x2-x0)*(x2-x1))
       dl0 = coerce $ (2*x-x1-x2)/((x0-x1)*(x0-x2))
       dl1 = coerce $ (2*x-x0-x2)/((x1-x0)*(x1-x2))
       dl2 = coerce $ (2*x-x0-x1)/((x2-x0)*(x2-x1))
   in  (y0*l0 + y1*l1 + y2*l2, y0*dl0 + y1*dl1 + y2*dl2)

parabolaBasisDerivativeLeft,
   parabolaBasisDerivativeCenter,
   parabolaBasisDerivativeRight ::
      (Fractional a, Coercible a b) => a -> a -> a -> b
parabolaBasisDerivativeLeft   x0 x1 x2 = coerce $ (x1-x2)/((x0-x1)*(x0-x2))
parabolaBasisDerivativeCenter x0 x1 x2 = coerce $ 1/(x1-x0) + 1/(x1-x2)
parabolaBasisDerivativeRight  x0 x1 x2 = coerce $ (x1-x0)/((x2-x0)*(x2-x1))

parabolaDerivativeCenterNode ::
   (Fractional a, Num b, Coercible a b) => (a,b) -> (a,b) -> (a,b) -> b
parabolaDerivativeCenterNode (x0,y0) (x1,y1) (x2,y2) =
   y0 * (parabolaBasisDerivativeLeft   x0 x1 x2) +
   y1 * (parabolaBasisDerivativeCenter x0 x1 x2) +
   y2 * (parabolaBasisDerivativeRight  x0 x1 x2)


parabola2ndDerivativeCenterNode :: (Fractional a, Num b, Coercible a b, Fractional b) => (a,b) -> (a,b) -> (a,b) -> (a,b) -> b
parabola2ndDerivativeCenterNode (xl,yl) (x0,y0) (x1,y1) (x2,y2) =
   let dy0 =
          yl * (coerce $ (x0-x1)/((xl-x0)*(xl-x1))) +
          y0 * (coerce $ (1/(x0-xl) + 1/(x0-x1))) +
          y1 * (coerce $ (x0-xl)/((x1-xl)*(x1-x0)))
       dy1 =
          y0 * (coerce $ (x1-x2)/((x0-x1)*(x0-x2))) +
          y1 * (coerce $ (1/(x1-x0) + 1/(x1-x2))) +
          y2 * (coerce $ (x1-x0)/((x2-x0)*(x2-x1)))
       d = (y1-y0)/ coerce (x1-x0)
       x = coerce x0
   in  2*(dy0-d) / (coerce $ sqr (x0-x1)) * (coerce (3*x-2*x1-x0)) +
       2*(dy1-d) / (coerce $ sqr (x1-x0)) * (coerce (3*x-2*x0-x1))
