# scala-fib
Fibonacci number calculation

Two implementations are present. One is traditional recursive algorithm. 
The other is an algorithm that uses matrix multiplication.
The idea and explanation of the algorithm can be found for instance in 
https://www.nayuki.io/page/fast-fibonacci-algorithms 

Due to the fact that this algorithm works with symmetric matrices,
a specialized matrix class has been introduced.
And in order to reuse algorithm `pow` from `spire` we
implement multiplicative semigroup type class.
