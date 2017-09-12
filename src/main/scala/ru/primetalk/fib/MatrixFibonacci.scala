package ru.primetalk.fib

import algebra.ring.MultiplicativeSemigroup

/**
  * An implementation of fibonacci numbers algorithm with performance
  * {{{O(Log(n))}}}.
  *
  */
object MatrixFibonacci {
  /** Condensed representation of a symmetric matrix 2x2 (a,b;b,c).
    * We just need to implement multiplication.
   */
  case class SymmetricMat22[Number](a: Number, b: Number, c: Number)

  implicit def symmetricMat22IsMultiplicativeSemigroup[Number: Numeric]: MultiplicativeSemigroup[SymmetricMat22[Number]] = new MultiplicativeSemigroup[SymmetricMat22[Number]] {
    private val num = implicitly[Numeric[Number]]
    /** Product of symmetric matrices is again symmetric. So we can use the same
      * condensed matrix representation.
      * */
    override def times(x: SymmetricMat22[Number], y: SymmetricMat22[Number]): SymmetricMat22[Number] = {
      val be = num.times(x.b, y.b)
      SymmetricMat22[Number](
        num.plus(num.times(x.a, y.a), be),
        num.plus(num.times(x.a, y.b), num.times(x.b, y.c)),
        num.plus(be, num.times(x.c, y.c))
      )
    }
  }
  /**
    * @tparam Number sufficient number representation
    * @return n-th Fibonacci number
    */
  final def apply[Number: Numeric](n: Int)(f0: Number, f1: Number): Number = {
    val num = implicitly[Numeric[Number]]

    val mat = implicitly[MultiplicativeSemigroup[SymmetricMat22[Number]]]

    type PosInt = Int

    val producerMatrix = SymmetricMat22[Number](num.one, num.one, num.zero) // we'll power this matrix

    def apply0(n: PosInt)(f0: Number, f1: Number): Number = {
      mat.pow(producerMatrix, n).a
    }

    n match {
      case _ if n == 0 =>
        f0
      case _ if n > 0 =>
        apply0(n - 1)(f0, f1)
      case _ if (-n + 1) % 2 == 0 =>
        apply0(-n + 1)(f0, f1)
      case _ =>
        num.negate(apply0(-n + 1)(f0, f1))
    }
  }

}

