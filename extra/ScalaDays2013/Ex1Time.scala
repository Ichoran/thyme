// Ex1Time.scala Copyright Rex Kerr 2013 / BSD 2-clause

object Ex1Time {
  val th = new ichi.bench.Thyme
  val dict = Seq(
    "salmon", "cod", "grouper", "bass", "herring",
    "eel", "trout", "perch", "halibut", "dorado"
  )
  def permuted = th.ptime{ dict.permutations.map(_.mkString).to[Vector] }
  def scanAll(sought: Seq[String]) = {
    def scan(s: String) = sought.exists(s contains _)
    val p = permuted; th.ptime{ p.filter(scan) }
  }
  def report(sought: Seq[String], scanned: Seq[String]) = th.ptime{
    sought map { word =>
      scanned find(_ contains word) match {
        case Some(s) => s"found $word in $s"
        case None    => s"could not find $word"
      }
    }
  }
  def printOut(lines: Seq[String]) = th.ptime{ lines.foreach(println) }
  def main(args: Array[String]) {
    val answer = report(args, scanAll(args))
    printOut(answer)
  }
}
