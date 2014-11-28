import scala.math.BigInt

object Euler25 {
  // lame I guess, but these are instantaneous with big int packages
  def apply() = {
    val big = BigInt("1" + (1 to 999).map(_ => "0").mkString(""))
    var f1 = BigInt(1)
    var f2 = BigInt(1)
    var k = 2
    while(f2 < big){
      val tmp = f1+f2
      f1=f2
      f2=tmp
      k+=1
    }
    k
  }
}
