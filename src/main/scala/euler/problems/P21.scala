package euler
package problems

object P21 extends Problem {
  val id = 21
  val name = "Amicable pairs"

  val MAX = 10000

  def solve {
    // index 0 is never used
    val sums = Array.fill((MAX+1) * 4)(0)

    for(i <- 1 until ((MAX+1) * 4)) {
      sums(i) = d(i)
    }

    var sum = 0
    for(i <- 1 to MAX) {
      val s = sums(i)
      if(sums(s) == i && s != i)  {
        println("pair : " + i + " : " + s)
        sum += i
      }
    }
    println(sum)
  }

  def d(n : Int) : Int = Common.properDivisors(n).sum
}
