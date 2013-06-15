// ProfEx1.scala Copyright Rex Kerr 2013 / BSD 2-clause

object ProfEx1 {
  val dict = Seq(
    "salmon", "cod", "grouper", "bass", "herring",
    "eel", "trout", "perch", "halibut", "dorado"
  )
  def permuted = dict.permutations.map(_.mkString).to[Vector]
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
