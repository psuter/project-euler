package euler
package problems

object P25 extends Problem {
  val id = 25
  val name = "First Fibonacci number with >= 1000 digits"

  def solve {
    // the + 2 is to:
    //   - pretend there's an extra 1 in the sequence 
    //   - account for 0-indexing
    println(fib.zipWithIndex.find(p => p._1.toString.length >= 1000).get._2 + 2)
  }

  def fib = new Iterator[BigInt] {
    val hasNext = true
    var last1 : BigInt = 0
    var last2 : BigInt = 1

    def next : BigInt = {
      val result = last1 + last2
      last1 = last2
      last2 = result
      result
    }
  }
}
