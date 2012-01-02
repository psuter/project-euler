package euler
package problems

object P24 extends Problem {
  val id = 24
  val name = "Millionth lexicographic permumation of 0..9"

  val target = 1000000

  def solve {
    var skippedPerm = 0
    var usedDigits : Set[Int] = Set.empty
    val digits = for(pos <- (0 to 9)) yield {
      val fact = Common.factorial(9 - pos).intValue // (never > 9!)
      val d = (target - skippedPerm) / fact
      skippedPerm += d * fact
      val digit = digitSkipping(d, usedDigits)
      usedDigits += digit
      digit
    }

    println(digits.mkString(""))
  }

  def digitSkipping(id : Int, skipSet : Set[Int]) : Int = {
    (0 to 9).filterNot(skipSet(_))(id)
  }
}
