// BenchEx1.scala Copyright Rex Kerr 2013 / BSD 2-clause

object BenchEx1 {
  val dict = Seq(
    "salmon", "cod", "grouper", "bass", "herring",
    "eel", "trout", "perch", "halibut", "dorado"
  )
  val cdict = dict.map(_.toCharArray).toArray
  val n = cdict.map(_.length).sum
  def main(args: Array[String]) {
    val th = new ichi.bench.Thyme
    val a = th.Warm{ dict.mkString }
    val b = th.Warm{
      val c = new Array[Char](n)
      var i,j = 0
      while (i < cdict.length) {
        System.arraycopy(cdict(i), 0, c, j, cdict(i).length)
        j += cdict(i).length
        i += 1
      }
      new String(c)
    }
    th.pbenchOffWarm()(a, wtitle="mkString")(b, vtitle="charcat")
  }
}
