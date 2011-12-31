package euler
package problems

object P17 extends Problem {
  val id = 17
  val name = "Letters to write down 1 to 1000 in English"

  def solve {
    println((1 to 1000).map(i => spellOut(i).filter(c => c != ' ' && c != '-').length).sum)
  }

  def spellOut(n : Int) : String = n match {
    case 0 => "zero"
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case 4 => "four"
    case 5 => "five"
    case 6 => "six"
    case 7 => "seven"
    case 8 => "eight"
    case 9 => "nine"
    case 10 => "ten"
    case 11 => "eleven"
    case 12 => "twelve"
    case 13 => "thirteen"
    case 14 => "fourteen"
    case 15 => "fifteen"
    case 16 => "sixteen"
    case 17 => "seventeen"
    case 18 => "eighteen"
    case 19 => "nineteen"
    case 20 => "twenty"
    case 30 => "thirty"
    case 40 => "forty"
    case 50 => "fifty"
    case 60 => "sixty"
    case 70 => "seventy"
    case 80 => "eighty"
    case 90 => "ninety"
    case x if x > 20 && x <= 99 =>
      spellOut(x - (x % 10)) + "-" + spellOut(x % 10)
    case x if x >= 100 && x <= 999 => {
      val hf = x / 100
      spellOut(hf) + " hundred" + (
        if(x % 100 == 0) 
          ""
        else
          " and " + spellOut(x % 100)
      )
    }
    case x if x >= 1000 && x <= 9999 => {
      val tf = x / 1000
      spellOut(tf) + " thousand" + (
        if(x % 1000 == 0)
          ""
        else
          " " + spellOut(x % 1000)
      )
    }
  }
}
