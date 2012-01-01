package euler

object Common {
  import scala.math._

  def properDivisors(n : Int) : Set[Int] = {
    val max = sqrt(n).toInt
    val smalls = (2 to max).filter(i => n % i == 0)
    val bigs = smalls.map(n / _)
    (smalls.toSet ++ bigs.toSet + 1) - n
  }
}
