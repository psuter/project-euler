package euler

object Common {
  import scala.math._

  def factorize(n : Int) : Map[Int,Int] = {
    @annotation.tailrec
    def f(n : Int, acc : Map[Int,Int]) : Map[Int,Int] = {
      if(n == 1) {
        Map.empty
      } else {
        Map.empty    
      }
    }

    f(n, Map.empty)
  }

  def properDivisors(n : Int) : Set[Int] = {
    val max = sqrt(n).toInt
    val smalls = (2 to max).filter(i => n % i == 0)
    val bigs = smalls.map(n / _)
    (smalls.toSet ++ bigs.toSet + 1) - n
  }

  def factorial(n : Int) : BigInt = {
    @annotation.tailrec
    def f(n : Int, acc : BigInt = 1) : BigInt = {
      if(n <= 1) {
        acc
      } else {
        f(n - 1, n * acc)
      }
    }

    val result = f(n)
    println("factorial(" + n + ") = " + result)
    result
  }
}
