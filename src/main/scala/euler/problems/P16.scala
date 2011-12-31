package euler
package problems

object P16 extends Problem {
  val id = 16
  val name = "Sum of digits of 2^1000"

  def solve {
    val theNum = fastExp(2, 1000)
    println(theNum)
    val sum = theNum.toString.toSeq.map(c => (c - '0').toInt).sum
    println(sum)
  }

  @annotation.tailrec
  def fastExp(n : BigInt, exp : Int, acc : BigInt = 1) : BigInt = {
    if(exp == 0) {
      acc
    } else if(exp == 1) {
      n * acc
    } else if(exp % 2 == 0) {
      fastExp(n * n, exp / 2, acc)
    } else {
      fastExp(n, exp - 1, acc * n)
    }
  }
}
