package ru.primetalk.fib

import algebra.ring.MultiplicativeSemigroup
import spire.algebra.{AdditiveAbGroup, MultiplicativeMonoid}

/**
  * An implementation of fibonacci numbers algorithm with performance
  * {{{O(Log(n))}}}.
  *
  */
object MatrixFibonacci {

  /**
    * @tparam Number sufficient number representation
    * @return n-th Fibonacci number
    */
  final def apply[Number: MultiplicativeMonoid: AdditiveAbGroup](n: Int)(f0: Number, f1: Number): Number = {
    val m = implicitly[MultiplicativeMonoid[Number]]
    val a = implicitly[AdditiveAbGroup[Number]]

    val mat = implicitly[MultiplicativeSemigroup[SymmetricMat22[Number]]]

    type PosInt = Int

    val producerMatrix = SymmetricMat22[Number](m.one, m.one, a.zero) // we'll power this matrix

    def powA(n: PosInt): Number = mat.pow(producerMatrix, n).a

    n match {
      case _ if n == 0 =>
        f0
      case _ if n == 1 || n == -1 =>
        f1
      case _ if n > 0 =>
        powA(n - 1)
      case _ if (-n + 1) % 2 == 0 =>
        powA(-n + 1)
      case _ =>
        a.negate(powA(-n + 1))
    }
  }

}

