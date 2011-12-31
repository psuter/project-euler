package euler
package problems

object P1 extends Problem {
  val id = 1
  val name = "Sum of multiples of 3 and 5 below 1000"

  def solve = {
    println(
      (1 to 1000).filter(n => n % 3 == 0 || n % 5 == 0).sum
    )
  }
}
