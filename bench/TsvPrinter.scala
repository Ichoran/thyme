package ichi.bench

import ichi.bench.Thyme.Benched
import java.io.{PrintWriter, File}

/**
 * Util for writing the results of a benchmark to a tsv file, so they can
 * be easily read in by an external program, eg. for graphing
 */
class TsvPrinter(val th: Thyme, val file: File) {
  val out = new PrintWriter(file)
  out.println("title\truntime")

  def addToTsv(benched: Benched) {
    benched.runtimeResults.data.value.foreach{runtime =>
      out.println(benched.title + "\t" + (runtime / benched.runtimeEffort))
    }
  }

  def close() {out.close()}

  /**
   * Run the benchmark and print the answer, like pbench, but also write
   * the times from each run to the tsv file
   */
  def tsvBench[A](f: => A, effort: Int = 1, title: String = "") = {
    val br = Thyme.Benched.empty
    br.title = title
    val ans = th.bench(f)(br, effort = effort)
    println(br)
    addToTsv(br)
    ans
  }

  /**
   * Print out a command you can paste into R to generate a boxplot
   */
  def showRCommand = println(
    """data <- read.table("""" + file.getAbsolutePath +
      """", sep="\t",h=T)
        |# you might want to reorder the plot with something like:
        |# data$title = factor(data$title, levels = c(<desired order>), ordered=T)
        |boxplot(runtime ~ title, data, ylab="seconds")
      """.stripMargin
  )
}

