// Ex2.scala Copyright Rex Kerr 2013 / BSD 2-clause

object Ex2 {
  val dict = Seq(
    "salmon", "cod", "grouper", "bass", "herring",
    "eel", "trout", "perch", "halibut", "dorado"
  )
  val cdict = dict.map(_.toCharArray).toArray
  val n = cdict.map(_.length).sum
  def permuted = cdict.permutations.map{ cs =>
    val c = new Array[Char](n)
    var i,j = 0
    while (i < cs.length) {
      val x = cdict(i)
      System.arraycopy(x, 0, c, j, x.length)
      j += x.length
      i += 1
    }
    new String(c)
  }.to[Vector]
  def scanAll(sought: Seq[String]) = {
    def scan(s: String) = sought.exists(s contains _)
    permuted.filter(scan)
  }
  def report(sought: Seq[String], scanned: Seq[String]) = sought map { word =>
    scanned find(_ contains word) match {
      case Some(s) => s"found $word in $s"
      case None    => s"could not find $word"
    }
  }
  def printOut(lines: Seq[String]) = lines.foreach(println)
  def main(args: Array[String]) {
    val answer = report(args, scanAll(args))
    printOut(answer)
  }
}
