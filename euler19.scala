object Euler19 {
  val firstDays = Array(31,28,31,30,31,30,31,31,30,31,30).scanLeft(1)((s,v) => (s+v) % 7) // 1 = Monday, 1900 non-leap

  val leapIncrement = (0 until 12).map{m => if (m ==0 | m == 1) 366 else 365}
  val nonLeapIncrement = (0 until 12).map(_ => 365)

  def isLeap(year: Int) = 
    ((year % 4) == 0) && !(((year % 100) == 0) && !((year % 400) == 0))

  def incrementFirstDays(prev: Array[Int], leap: Boolean = false) = {
    (0 until 12).foreach{
      idx =>
        prev(idx) += {if (leap) leapIncrement(idx) else nonLeapIncrement(idx)}
        prev(idx) %= 7
    }
  }

  var mondays = 0
  (1900 until 2000).foreach{
    year => 
      incrementFirstDays(firstDays, isLeap(year))
      mondays += firstDays.filter(_ == 0).length
  }

  def apply() = {println(mondays)}
}

