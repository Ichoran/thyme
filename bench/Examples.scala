package ichi.bench.examples

import ichi.bench._

/** Example of different modes of usage of Parsley. */
object ParsleyExample {
  val th = new Thyme
  
  def L(l: Long) = l+(l-1)-(l>>>2)    // Try to foil inlining so we get a stack trace hit
  def M(l: Long) = (l>>>2)+(l>>>3)
  def method1(s: String, l: Long) = { var i=0; var k=0L; while (i<10000) { k += s.length*(l+i); i += 1 }; M(L(k)) }
  def method2(s: String, l: Long) = { var i=0; var k=0L; while (i<1000) { k += s.charAt(((l+i)%s.length).toInt).toLong; i += 1 }; M(L(k)) }
  def method3(s: String, l: Long) = { var k = l; var i = 0; while (i<s.length) { k += s.charAt(i); i += 1 }; i=0; while (i<10000) { k+=i*i; i += 1}; M(L(k)) }
  val methodXer = Parsley.methods[(String,Long), Long].add((method1 _).tupled).add((method2 _).tupled).add((method3 _).tupled)
  def method(s: String, l: Long) = methodXer.run((s,l))
  
  def main(args: Array[String]) {
    // Parsley switches methods itself
    val p = methodXer.parsley
    p.go
    var l = 0L
    for (i <- 1 to 500000) l += method(i.toString, i*i.toLong)
    p.stop
    p.reportStats(perEntry = true)
    p.quit
    println
    
    // Parsley looks at stack traces--beware of inlining!  Seeking works best for heavy methods that can appear in a stack trace
    // Here, we look at the calling lines since method1-3 can get inlined (but the anon function to the for loop does not)
    // This is ridiculously fragile!  Be careful!  Almost anything can throw this off!
    val q = Parsley.seek("one",false,".*Example.scala:11.*".r).and("two",false,".*Example.scala:12.*".r).and("three",false,".*Example.scala:13.*".r).parsley
    q.go
    for (i <- 1 to 500000) {
      val s = i.toString
      val k = i*i.toLong
      // The following nonsense is to try to prevent inlining of the methods so we can get a stack trace
      val o1 = Option((i%3).toLong).filter(_ != 0).orElse(Some(method1(s,k)))
      val o2 = o1.filter(_ != 1).orElse(Some(method2(s,k)))
      val o3 = o2.filter(_ != 2).orElse(Some(method3(s,k)))
      l += o3.get
    }
    q.stop
    q.reportStats()  // No per-entry timing here, as method entries are not counted!
    q.quit
    println
    
    // Keep explicit track of entry/exit from methods and let Parsley detect
    val r = new Parsley(3)
    r.go
    for (i <- 1 to 500000) {
      val s = i.toString
      val k = i*i.toLong
      l += ((i%3) match {
        case 0 => r.call(1)(())(_ => method1(s,k))
        case 1 => r.call(2)(())(_ => method2(s,k))
        case _ => r.call(3)(())(_ => method3(s,k))
      })
    }
    r.stop
    r.reportStats()  // There is per-entry timing, but we'll skip it
    r.quit
    println
    
    // Old fashioned run-the-method-in-a-timing-loop for comparison
    print("Method 1\n  "); th.ptime({ var l = 0L; var i = 0; while (i < 500000) { l += method1(i.toString, i*i.toLong); i += 1 }; l })
    print("Method 2\n  "); th.ptime({ var l = 0L; var i = 0; while (i < 500000) { l += method2(i.toString, i*i.toLong); i += 1 }; l })
    print("Method 3\n  "); th.ptime({ var l = 0L; var i = 0; while (i < 500000) { l += method3(i.toString, i*i.toLong); i += 1 }; l })
  }
}


/** Example of different modes of usage of Thyme. */
object ThymeExample {
  def main(args: Array[String]) {
    val rng = (new ichi.maths.Rng.Hybrid2).seedWithTime
    val th = Thyme.warmed(verbose = print)
    
    println("Raw timing of creation of a 1000-int set, not warmed up.")
    th.pclock((1 to 1000).toSet)

    println("\nMapping and summing (generic) over an array from 0 until 1000 cold (no warmup):")
    th.ptime(Array.range(0, 1000).map(i => i*i).sum)
    
    println("\nWarming the array sum, then timing:")
    val wa = th.Warm(Array.range(0, 1000).map(i => i*i).sum)
    th.ptimeN(wa())(wa.reps, 1, wa.combine)
    
    println("\nBenchmarking of 1000-element list sum, no additional warmup:")
    th.pbench(List.range(0,1000).map(i => i*i).sum)
    
    println("\nBenchmarking of 1000-element vector sum with pre-warmup:")
    val wv = th.Warm(Vector.range(0,1000).map(i => i*i).sum)
    th.pbenchWarm(wv)
    
    println("\nHead-to-head benchmark of generic array sum vs. vector sum, no pre-warmup:")
    th.pbenchOff(){ Array.range(0, 1000).map(i => i*i).sum }{ Vector.range(0, 1000).map(i => i*i).sum }
    
    println("\nHead-to-head benchmark of pre-warmed generic array sum vs. vector sum:")
    th.pbenchOffWarm()(wa)(wv)
    
    println("\nComputational complexity of primitive array sum")
    th.porder(
      Thyme.Resource{ n => var i=0; val a = new Array[Int](n); while (i < n) { a(i) = i; i += 1 }; a }
    )(
      Thyme.Resource.inline((start, n, data) => { var s = 0L; var i = 0; while (i < n) { s += data(i+start)*data(i+start); i += 1 }; s })
    )(
      8192
    )
    
    println("\nWhat's the fastest way to sum a vector?\nClosure inside foreach or iterator in while loop?")
    val myVector = Vector.fill(2048)(rng.roll(1000))
    val cTrav = th.Warm{ var s = 0; myVector.foreach{ i => s += i }; s }
    val iWhil = th.Warm{ var s = 0; val it = myVector.iterator; while (it.hasNext) s += it.next; s }
    th.pbenchOffWarm(title = "Traversable vs. Iterable in Vector")(cTrav, 2048, "Closure in foreach")(iWhil, 2048, "Iterator-while")
    
    println("\nWait, do we agree with ourselves?  Let's try it again.")
    th.pbenchOffWarm(title = "Traversable vs. Iterable in Vector")(cTrav, 2048, "Closure in foreach")(iWhil, 2048, "Iterator-while")  
  }
}
