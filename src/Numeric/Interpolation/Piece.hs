module Numeric.Interpolation.Piece (
   Piece.T,
   linear,
   hermite1,
   ) where

import qualified Numeric.Interpolation.Private.Piece as Piece
import Data.Coerce (Coercible, coerce)


{- $setup
>>> import qualified Numeric.Interpolation.Piece as Piece
>>> import qualified Numeric.Interpolation.Private.Piece as PiecePriv
>>> import qualified Test.QuickCheck as QC
>>> import Test.QuickCheck ((==>))
>>>
>>> forAllDistinctPoints ::
>>>    (Show a, QC.Arbitrary a, QC.Testable prop) =>
>>>    ((Rational, a) -> (Rational, a) -> prop) -> QC.Property
>>> forAllDistinctPoints f =
>>>    QC.forAll QC.arbitrary $ \p1@(x1,_) ->
>>>    QC.forAll QC.arbitrary $ \p2@(x2,_) ->
>>>       x1/=x2  ==>  f p1 p2
-}

{- |
prop> forAllDistinctPoints $ \p1 p2 x -> Piece.linear p1 p2 x == Piece.linear p2 p1 x
-}
linear :: (Fractional a, Fractional b, Coercible a b) => Piece.T a b b
linear = Piece.linear

{- |
Hermite interpolation with one derivative per node.
That is, the interpolating polynomial is cubic.

prop> forAllDistinctPoints $ \p1 p2 x -> Piece.hermite1 p1 p2 x == Piece.hermite1 p2 p1 x
prop> forAllDistinctPoints $ \p1@(x1,y1) p2@(x2,y2) x -> Piece.linear p1 p2 x == let slope = (y2-y1)/(x2-x1) in Piece.hermite1 (x1, (y1,slope)) (x2, (y2,slope)) x
prop> forAllDistinctPoints $ \p1 p2 x -> Piece.hermite1 p1 p2 x == PiecePriv.hermite1 p1 p2 x
-}
hermite1 :: (Fractional a, Fractional b, Coercible a b) => Piece.T a b (b, b)
hermite1 (x0,(y0,dy0)) (x1,(y1,dy1)) x =
   let d = (y1-y0) / coerce dx10
       dx0 = x-x0
       dx1 = x1-x
       dx10 = x1-x0
   in  (y0* coerce dx1 + y1 * coerce dx0 +
        ((dy0-d) * coerce dx1 - (dy1-d) * coerce dx0) * coerce dx0 * coerce dx1 / coerce dx10)
          / coerce dx10
