package ru.primetalk.fib

import org.scalatest.Matchers

class MatrixFibonacciTest extends org.scalatest.FlatSpec with Matchers {
  behavior of "MatrixFibonacci"
  it should "give the same results as Recursive" in {
    import spire.math.Numeric.IntIsNumeric

    RecursiveFibonacci(0)(0, 1) shouldBe 0
    RecursiveFibonacci(1)(0, 1) shouldBe 1
    RecursiveFibonacci(2)(0, 1) shouldBe 1
    RecursiveFibonacci(3)(0, 1) shouldBe 2
    RecursiveFibonacci(4)(0, 1) shouldBe 3
    RecursiveFibonacci(5)(0, 1) shouldBe 5

    MatrixFibonacci(0)(0, 1) shouldBe 0
    MatrixFibonacci(1)(0, 1) shouldBe 1
    MatrixFibonacci(2)(0, 1) shouldBe 1
    MatrixFibonacci(3)(0, 1) shouldBe 2
    MatrixFibonacci(4)(0, 1) shouldBe 3
    MatrixFibonacci(5)(0, 1) shouldBe 5

    for{ n <- 10 to 30 } {
      MatrixFibonacci(n)(0, 1) shouldBe RecursiveFibonacci(n)(0, 1)
    }
  }
  
  it should "give the same results as Recursive for big numbers" in {
    import spire.math.Numeric.BigIntIsNumeric

    val n = 100000

    MatrixFibonacci(n)(BigInt(0), BigInt(1)) shouldBe RecursiveFibonacci(n)(BigInt(0), BigInt(1))
  }
}