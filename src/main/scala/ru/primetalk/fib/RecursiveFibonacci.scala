package ru.primetalk.fib

import spire.algebra.AdditiveAbGroup

import scala.annotation.tailrec

/**
  * An implementation of fibonacci numbers algorithm with performance {{{O(n)}}}.
  */
object RecursiveFibonacci {

  /**
    * @tparam Number sufficient number representation
    * @return
    */
  final def apply[Number: AdditiveAbGroup](n: Int)(f0: Number, f1: Number): Number = {
    type PosInt = Int

    @tailrec
    def apply0(n: PosInt)(f0: Number, f1: Number): Number = n match {
      case 0 => f0
      case 1 => f1
      case _ if n > 1 =>
        apply0(n - 1)(f1, implicitly[AdditiveAbGroup[Number]].plus(f0, f1))
    }
    n match {
      case _ if n >= 0 =>
        apply0(n)(f0, f1)
      case _ if (-n) % 2 == 0 =>
        apply0(-n)(f0, f1)
      case _ =>
        implicitly[AdditiveAbGroup[Number]].negate(apply0(-n)(f0, f1))
    }
  }
}
