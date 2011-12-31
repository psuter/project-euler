package euler
package problems

object P18 extends Problem {
  val id = 18
  val name = "Best route in a triangle"

  def solve = println(maxInTriangle(theTriangle))

  def maxInTriangle(triangle : String) : Int = {
    val a = parseTriangle(triangle) 

    for(i <- 1 until a.length; val row = a.length - 1 - i) {
      for(col <- 0 to row) {
        val b1 = a(row + 1)(col)
        val b2 = a(row + 1)(col + 1)
        a(row)(col) = a(row)(col) + (b1 max b2)
      }
    }

    a(0)(0)
  }

  val smallTriangle = """
  3
  7 4
  2 4 6
  8 5 9 3
  """

  val theTriangle = """
  75
  95 64
  17 47 82
  18 35 87 10
  20 04 82 47 65
  19 01 23 75 03 34
  88 02 77 73 07 63 67
  99 65 04 28 06 16 70 92
  41 41 26 56 83 40 80 70 33
  41 48 72 33 47 32 37 16 94 29
  53 71 44 65 25 43 91 52 97 51 14
  70 11 33 28 77 73 17 78 39 68 17 57
  91 71 52 38 17 14 91 43 58 50 27 29 48
  63 66 04 68 89 53 67 30 73 16 69 87 40 31
  04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
  """

  // This assumes the triangle is well-formed in many ways...
  def parseTriangle(in : String) : Array[Array[Int]] = {
    val lines = in.split("\n").map(_.trim).filter(!_.isEmpty)
    val size = lines.length
    val array = Array.tabulate(size)(n => Array.fill(n+1)(0))
    for((l,i) <- lines.zipWithIndex) {
      val elements = l.split(" ").filter(!_.isEmpty)
      for((e,j) <- elements.zipWithIndex) {
        array(i)(j) = e.toInt
      }
    }
    array
  }
}
