package euler
package problems

object P14 extends Problem {
  val id = 14
  val name = "Max Collatz sequence starting under 1000000"

  val MAX = 1000000
  val ARR = 4000000

  def solve = {
    val steps : Array[Int] = Array.fill(ARR)(-1)
    steps(1) = 0
    steps(0) = 0

    // Computes the number of required steps, using the cache...
    @annotation.tailrec
    def cacheCompute(n : BigInt, trail : List[BigInt] = Nil) : Int = {
      if(n < ARR) {
        // It always finishes here, since it must finish at 1.
        if(steps(n.intValue) != -1) {
          var s = steps(n.intValue)
          var t = trail
          while(!t.isEmpty) {
            val h = t.head
            t = t.tail
            s += 1
            if(h < ARR) {
              steps(h.intValue) = s
            }
          }
          s
        } else {
          cacheCompute(collatzNext(n), n :: trail)
        }
      } else {
        cacheCompute(collatzNext(n), n :: trail)
      }
    }

    def fillFrom(n : Int) : Unit = {
      var stack : List[Int] = n :: Nil

      while(!stack.isEmpty) {
        val head = stack.head
        stack = stack.tail

        val s = steps(head)
        assert(s != -1)
        val t1 = 2 * head
        if(t1 < ARR) {
          assert(steps(t1) == -1)
          steps(t1) = s + 1
          stack = t1 :: stack
        }

        if((head - 1) % 3 == 0) {
          val t2 = (head - 1) / 3
          if(t2 % 2 == 1 && t2 != 1) {
            assert(steps(t2) == -1)
            steps(t2) = s + 1
            stack = t2 :: stack
          }
        }
      }
    }

    println("Prefilling...")
    fillFrom(1)

    println("Filling the holes...")
    for(n <- 0 until MAX) {
      if(steps(n) == -1) {
        cacheCompute(n)
      }
    }

    println("Finding the max...")
    var max   = Int.MinValue
    var maxId = 0
    for(n <- 0 until MAX) {
      if(steps(n) > max) {
        maxId = n
        max = steps(n)
      }
    }

    println("Max steps: " + max + ", starting at: " + maxId)
  }

  def collatzNext(n : BigInt) : BigInt =
    if(n % 2 == 0) (n / 2) else (3 * n + 1)
}
