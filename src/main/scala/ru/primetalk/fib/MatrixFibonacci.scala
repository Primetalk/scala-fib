package ru.primetalk.fib

import scala.annotation.tailrec

/**
  * An implementation of fibonacci numbers algorithm with performance
  * {{{O(Log(n))}}}.
  *
  */
object MatrixFibonacci {
  /** Condensed representation of a symmetric matrix 2x2 (a,b;b,c).
   */
  case class SymmetricMat22[Number](a: Number, b: Number, c: Number)

  /**
    * @tparam Number sufficient number representation
    * @return
    */
  final def apply[Number: Numeric](n: Int)(f0: Number, f1: Number): Number = {
    val num = implicitly[Numeric[Number]]

    type PosInt = Int
    type Mat = SymmetricMat22[Number]
    def Mat(a: Number, b: Number, c: Number) = SymmetricMat22[Number](a, b, c)

    val one = num.one
    val zero = num.zero
    val producerMatrix = SymmetricMat22[Number](one, one, zero) // we'll power this matrix
    val identityMatrix = SymmetricMat22[Number](one, zero, one)

    def mul(m1: Mat, m2: Mat): Mat = {
      val be = num.times(m1.b, m2.b)
      Mat(
        num.plus(num.times(m1.a, m2.a), be),
        num.plus(num.times(m1.a, m2.b), num.times(m1.b, m2.c)),
        num.plus(be, num.times(m1.c, m2.c))
      )
    }
    @tailrec
    def powAndMultLog(n: PosInt)(mat: Mat, multiplyAtTheEnd: Mat): Mat = n match {
      case 0 => multiplyAtTheEnd
      case 1 => mul(mat, multiplyAtTheEnd)
      case _ if n % 2 == 0 => powAndMultLog(n / 2)(mul(mat, mat), multiplyAtTheEnd)
      case _ if n % 2 == 1 => powAndMultLog(n / 2)(mul(mat, mat), mul(mat, multiplyAtTheEnd))
    }
    @tailrec
    def powAndMultLinear(n: PosInt)(mat: Mat, multiplyAtTheEnd: Mat): Mat = n match {
      case 0 => multiplyAtTheEnd
      case _ => powAndMultLinear(n-1)(mat, mul(mat, multiplyAtTheEnd))
    }

    def apply0(n: PosInt)(f0: Number, f1: Number): Number = {
      powAndMultLog(n)(producerMatrix, identityMatrix).a
    }
    n match {
      case _ if n == 0 =>
        f0
      case _ if n > 0 =>
        apply0(n-1)(f0, f1)
      case _ if (-n+1) % 2 == 0 =>
        apply0(-n+1)(f0, f1)
      case _ =>
        num.negate(apply0(-n+1)(f0, f1))
    }
  }

}
