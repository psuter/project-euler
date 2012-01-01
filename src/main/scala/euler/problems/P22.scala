package euler
package problems

object P22 extends Problem {
  val id = 22
  val name = "Names"

  def solve {
    import scala.io.Source
    val src = Source.fromInputStream(
      P22.getClass.getResourceAsStream("/22-names"))

    val names = src.getLines
      .flatMap(line =>
        line
          .split(",")
          .filter(!_.isEmpty)
          .map(n => n.substring(1, n.length - 1)))
    
    def nameValue(name : String) : Int = 
      name.toSeq.map(c => (c - '@') : Int).sum
    
    var sum : BigInt = 0
    for((n,i) <- names.toSeq.sorted.zipWithIndex) {
      sum += (i + 1) * nameValue(n)
    }

    println(sum)
  }
}
