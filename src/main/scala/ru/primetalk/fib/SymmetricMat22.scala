package ru.primetalk.fib

import algebra.ring.MultiplicativeSemigroup
import spire.algebra.Ring

/** Condensed representation of a symmetric matrix 2x2 (a,b;b,c).
  * We just need to implement multiplication.
  */
case class SymmetricMat22[Number](a: Number, b: Number, c: Number)

object SymmetricMat22 {
  implicit def symmetricMat22IsMultiplicativeSemigroup[Number: Ring]: MultiplicativeSemigroup[SymmetricMat22[Number]] = new MultiplicativeSemigroup[SymmetricMat22[Number]] {
    import spire.syntax.numeric.{additiveSemigroupOps, multiplicativeSemigroupOps}

    /** Product of symmetric matrices is again symmetric. So we can use the same
      * condensed matrix representation.
      */
    override def times(x: SymmetricMat22[Number], y: SymmetricMat22[Number]): SymmetricMat22[Number] = {
      val be = x.b * y.b
      SymmetricMat22[Number](
        x.a * y.a + be,
        x.a * y.b + x.b * y.c,
        be + x.c * y.c
      )
    }
  }

}