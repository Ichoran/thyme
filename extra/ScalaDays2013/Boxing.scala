// Boxing.scala Copyright Rex Kerr 2013 / BSD 2-clause

import ichi.bench.Thyme

import language.reflectiveCalls

object Boxing {
  final val N = 1000
  @volatile var mysteryIndex = 0
  def fillInts(a: Array[Int]) = { var i = 0; while (i<N) { a(i) = 1000000+i; i += 1 }; a(mysteryIndex) }
  def fillObjs(a: Array[Any]) = { var i = 0; while (i<N) { a(i) = 1000000+i; i += 1 }; a(mysteryIndex) }
  def summInts(a: Array[Int]) = { var i,s = 0; while (i<N) { s += a(i); i += 1 }; s }
  def summObjs(a: Array[Any]) = { var i,s = 0; while (i<N) { s += a(i).asInstanceOf[Int]; i += 1 }; s }
  class BoxedString(val s: String) {
    def length: Int = s.length
  }
  implicit class AbbrevStrLen(val s: String) extends AnyVal {
    def len = s.length
  }
  implicit class Abb2StrLen(val s: String) {
    def L = s.length
  }
  def sumRLen(a: Array[String]) = { var i,s = 0; while (i<N) { s += a(i).length; i += 1 }; s }
  def sumBLen(a: Array[BoxedString]) = { var i,s = 0; while (i<N) { s += a(i).length; i += 1 }; s }
  def sumXLen[A <: { def length(): Int }](a: Array[A]) = { var i,s = 0; while (i<N) { s += a(i).length; i += 1 }; s }
  def sumALen(a: Array[String]) = { var i,s = 0; while (i<N) { s += a(i).len; i += 1 }; s }
  def sum2Len(a: Array[String]) = { var i,s = 0; while (i<N) { s += a(i).L; i += 1 }; s }
  def main(args: Array[String]) {
    val th = Thyme.warmed(verbose = print)
    val ai = new Array[Int](N)
    val ao = new Array[Any](N)
    val wfi = th.Warm[Any](fillInts(ai))
    val wfo = th.Warm[Any](fillObjs(ao))
    th.pbenchOffWarm(title="Fill Int vs. Obj")(wfi,N)(wfo,N)
    val wsi = th.Warm(summInts(ai))
    val wso = th.Warm(summObjs(ao))
    th.pbenchOffWarm(title="Sum Int vs. Obj")(wsi,N)(wso,N)
    val rs = Array.tabulate(N)(i => i.toString)
    val bs = rs.map(s => new BoxedString(s))
    val wsr = th.Warm(sumRLen(rs))
    val wsb = th.Warm(sumBLen(bs))
    th.pbenchOffWarm(title="Pass through to string length")(wsr,N)(wsb,N)
    val wsx = th.Warm(sumXLen(rs))
    th.pbenchOffWarm(title="Structural typing")(wsr,N)(wsx,N)
    val wsa = th.Warm(sumALen(rs))
    th.pbenchOffWarm(title="Value class")(wsr,N)(wsa,N)
    val ws2 = th.Warm(sum2Len(rs))
    th.pbenchOffWarm(title="Implicit class")(wsr,N)(ws2,N)
  }
}
