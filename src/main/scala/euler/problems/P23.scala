package euler
package problems

object P23 extends Problem {
  val id = 23
  val name = "Sums of two abundant numbers"

  // Anything above that can be written as the sum of two 
  // abundant numbers.
  val MAX = 28123
  def solve {
    val abNums = (1 to MAX).filter(isAbundant).toSeq
    val array : Array[Boolean] = Array.fill(MAX + 1)(false)

    for(an1 <- abNums; an2 <- abNums) {
      val s = an1 + an2
      if(s <= MAX) {
        array(s) = true
      }
    }

    val theOnes = (1 to MAX).filter(!array(_))
//    println(abNums.take(40).mkString(", "))
//    println(theOnes.take(40).mkString(", "))
//    println(theOnes.last)
    
    println(theOnes.sum)
  }

  def isAbundant(n : Int) : Boolean = Common.properDivisors(n).sum > n
}
