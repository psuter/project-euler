package euler
package problems

object P92 extends Problem {
  val id = 92
  val name = "Number Chains"

  val MAX = 10000000
  def solve {
    val array = Array.fill(MAX + 1)(0)
    array(1) = 1
    array(89) = 89

    @annotation.tailrec
    def trace(n : Int, ints : List[Int] = Nil) : Unit = {
      val a = array(n)
      n match {
        case 1 | 89 => ints.foreach(array(_) = n)
        case _ if a != 0 => trace(a, ints) // shortcut !
        case _ => trace(next(n), n :: ints)
      }
    }

    (1 to MAX).foreach(trace(_))

    println((1 to MAX).count(array(_) == 89))
  }

  // Int is very safe... even 99999999 => 648...
  def next(n : Int) : Int = n.toString.toSeq.map(c => (c - '0').toInt).map(x => x * x).sum
}
