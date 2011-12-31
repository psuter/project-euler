package euler
package problems

object P19 extends Problem {
  val id = 19
  val name = "Sunday the 1st"

  def solve {
    var count = 0
    var day   = 1    // 0 == Sunday, 1 == Monday, etc.
    var month = 0    // 0 == January, etc.
    var year  = 1900

    val daysInMonth = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    while(year <= 2000) {
      if(day == 0 && year != 1900) // to skip 1900...
        count += 1

      day += daysInMonth(month) 
      if(month == 1 && isLeap(year)) {
        day += 1
      }
      day = day % 7

      month += 1
      if(month == 12) {
        year += 1
        month = 0
      }
    }

    println(count)
  }

  def isLeap(y : Int) : Boolean = 
    (y % 4 == 0) && (y % 100 != 0 || y % 400 == 0)
}
