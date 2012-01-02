package euler

object Main {
  def main(args : Array[String]) {
    if(args.length != 1) {
      println("Usage: euler N, where N is index of problem.")
      println("Available problems:")
      println(problems.keySet.toSeq.sorted.mkString(", "))
      sys.exit(1)
    }

    val n = args(0).toInt

    problems.get(n) match {
      case Some(p) => {
        assert(p.id == n) // ...
        println("Solving problem #" + p.id + "...")
        println(p.name)
        p.solve
        println("Done.")
      }
      case None => {
        println("Problem undefined : #" + n + ". Sorry.")
        sys.exit(1)
      }
    }
  }

  import euler.problems._
  val problems : Map[Int,Problem] = Map(1 -> P1, 14 -> P14, 16 -> P16, 17 -> P17, 18 -> P18, 19 -> P19, 21 -> P21, 22 -> P22, 23 -> P23, 24 -> P24, 25 -> P25, 67 -> P67, 92 -> P92)
   
}
