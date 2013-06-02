// ----------------------------------------------------------------------------
// Parsley is a lightweight local profiling assistant for Scala
// Copyright (c) 2013, Rex Kerr.  All rights reserved.
// Parsley is provided as open source software under the BSD 3-clause license
// See http://www.opensource.org/licenses/bsd-license.php for text
// ----------------------------------------------------------------------------



package ichi.bench

/**
 * Parsley is Profiling And Random Selection, Locally Evaluated.  Yay!
 * 
 * Parsley is an embedded mini-profiler that works by either evaluating stack traces
 * (with all the problems that entails), or provides convenient ways to manually
 * instrument calls of interest.  It works by spawning a thread that will either
 * collect stack traces or will read entry status for a predefined number of methods.
 * 
 * To collect stack traces, an instance of [[Parsley.StackSeeker]] must be provided
 * to select the names or other features of interest from within the stack trace.
 * 
 * See or run ichi.bench.examples.ParsleyExample for examples of usage.
 * 
 * `variants` is the number of slots to use to count instrumented methods.
 * `names` maps slot numbers to text descriptions (runs from 1 to variants inclusive)
 * Stack traces will be collected only if `stacked` is present.
 */
class Parsley (variants: Int, names: Int => String = (i: Int) => "Method "+i, stacked: Option[Parsley.StackSeeker] = None) { parsley => 
  if (variants < 1) throw new IllegalArgumentException("Must measure at least one thing; requested "+variants)
  private val counters = Array.fill(variants+1)(new java.util.concurrent.atomic.AtomicInteger(0))
  private val accumulator = Array.fill(variants+1)(0L)
  private val misses = Array.fill(variants+1)(0L)
  private val invocations = Array.fill(variants+1)(0L)
  
  /** This method keeps track of entry/exit into methods, so polling has something to poll.  You should instrument
   * method calls by replacing the normal call with this call, where `f` is your normal method call.  `i` is the index
   * under which to count this call.
   */
  def call[A,B](i: Integer)(a: A)(f: A => B): B = {
    if (Polling.quitNow) f(a) else
    {
      if (i<=0 || i>variants) throw new IllegalArgumentException("Methods must be numbered with positive integers, not "+i)
      if (Polling.active.get) invocations(i) += 1
      counters(i).incrementAndGet
      try { f(a) } finally { counters(i).decrementAndGet }
    }
  }
  
  /** A thread that sits there and polls to see what is being executed.  (This is how timing is collected.) */
  private object Polling extends Thread {
    var quitNow = false
    var started = false
    private var runCount = 0
    val ticks = new java.util.concurrent.atomic.AtomicInteger(0)
    val active = new java.util.concurrent.atomic.AtomicBoolean(false)
    override def start() {
      started = true
      super.start()
    }
    override def run() {
      val delay = {
        if (stacked.isDefined) {
          val dt = Array.fill(5){ val t0 = System.nanoTime; stacked.get.gather; System.nanoTime - t0 }.min
          math.min(100, math.max(1, ((10*dt)/1000000).toInt))  // Target < 10% processor use on one thread
        }
        else 1
      }
      while (!quitNow) {
        if (active.get) {
          val traces = (if (stacked.isDefined) stacked.get.gather else null)
          parsley.synchronized {
            var i = 0
            ticks.incrementAndGet
            while (i <= variants) {
              val n = counters(i).get() + (if (traces != null && i>0) traces(i-1) else 0)
              if (n > 0) { accumulator(i) += n }
              else { misses(i) += 1 }
              i += 1
            }
          }
        }
        Thread.sleep( delay )
      }
    }
  }
  
  /** Start polling. */
  def go: this.type = { if (!Polling.started) { Polling.start() }; Polling.active.set(true); this }
  
  /** Stop polling. */
  def stop: this.type = { Polling.active.set(false); this }
  
  /** Reset all statistics */
  def reset: this.type = synchronized {
    Polling.active.set(false)
    Polling.ticks.set(0)
    for (i <- counters.indices) { counters(i).set(0); accumulator(i) = 0 }
    this
  }
  
  /** Quit the polling thread. If you start the thread, you are responsible for calling this method to stop it to avoid thread leak. */
  def quit { Polling.quitNow = true }
  
  /** Calculate statistics as they currently stand */
  def getStats(multicount: Boolean = true): (Array[Float], Array[Float], Array[Long], Int) = {
    val (ticks, counts, zeros, entries) = parsley.synchronized{ (Polling.ticks.get, accumulator.clone, misses.clone, invocations.clone) }
    // Bayesian mean & error estimates
    def bayesEst(i: Long, n: Long) = (i+1f)/(n+2f)
    def bayesErr(i: Long, n: Long) = (math.sqrt((i+1f)*((n-i)+1f)/(n+3f))/(n+2f)).toFloat
    if (multicount) {
      // Semi-Bayesian; without histograms we can't really know if it's e.g. Poisson
      val estimates = (zeros.tail, counts.tail).zipped.map{ (z,c) => (if (c==0 || z>=ticks) 1f else (c.toFloat/(ticks-z)))*bayesEst(ticks-z, ticks) }
      val errors = (zeros.tail, counts.tail).zipped.map{ (z,c) => (if (c==0 || z>=ticks) 1f else (c.toFloat/(ticks-z)))*bayesErr(z, ticks) }
      (estimates, errors, entries.tail, ticks)
    }
    else {
      val estimates = zeros.tail.map(z => bayesEst(ticks-z, ticks))
      val errors = zeros.tail.map(z => bayesErr(z, ticks))
      (estimates, errors, entries.tail, ticks)
    }
  }
  
  /** Text output */
  def reportStats(perEntry: Boolean = false, multicount: Boolean = true, pr: String => Unit = println _) {
    val (estimates, errors, entries, ticks) = getStats(multicount)
    pr(s"Parsley profiling, $ticks ticks sampled")
    pr("Fractional time taken: ")
    (estimates zip errors).zipWithIndex.sortBy(- _._1._1).foreach{ case ((v,e),i) =>
      pr("  %7.3f%% +- %-8s = %s".format(v*100,"%7.3f%%".format(e*100).trim,names(i+1)))
    }
    if (perEntry) {
      pr("Relative time taken per entry (observed entries only): ")
      val data = ((estimates zip errors) zip entries).zipWithIndex.filter(_._1._2 > 0).sortBy(x => -x._1._1._1 / x._1._2)
      if (data.length > 0) {
        val (((v0,e0),n0),i0) = data.head
        val scale = n0/v0
        val scalerr = e0/v0
        data.foreach{ case (((v,e),n),i) =>
          val value = (v/n)*scale
          val e1 = (e/n)*scale
          val e2 = value*scalerr
          pr("  %7.3f%% +- %-8s = %s".format(value*100, "%7.3f%%".format(100*math.sqrt(e1*e1+e2*e2)).trim, names(i+1)))
        }
      }
    }
  }
  
  override def toString = { val sb = new StringBuilder; reportStats(pr = line => { sb ++= line; sb ++= "\n" }); sb.result }
}

object Parsley {
  /** This class holds information about what you want to find in stack traces.  `name` is used in output.
   * if `multiple` is true, then the recursive methods will count for one for each time they're on the stack.
   * `first` and `extra` are regular expressions that must match a stack trace in order for it to count.
   */
  class StackSeeker protected[Parsley] (name: String, multiple: Boolean, first: scala.util.matching.Regex, extra: scala.util.matching.Regex*) {
    private var myParsley: Parsley = null
    private var building = true
    private val threadMgr = java.lang.management.ManagementFactory.getThreadMXBean
    private val sought = new collection.mutable.ArrayBuffer[(String, Boolean, Seq[scala.util.matching.Regex])]
    sought += ((name, multiple, first +: extra))
    /** Add more names/regexes to search for another item in the stack. */
    def and(name: String, multiple: Boolean, first: scala.util.matching.Regex, extra: scala.util.matching.Regex*): this.type = synchronized {
      if (!building) throw new IllegalStateException("Cannot add items to monitor once gathering begins.")
      sought += ((name, multiple, first +: extra))
      this
    }
    /** Add more names/regexes, with multiple-counting enabled for this count. */
    def and(name: String, first: scala.util.matching.Regex, extra: scala.util.matching.Regex*): this.type = and(name, true, first, extra: _*)
    /** Add a name as a regex search. */
    def and(name: String, multiple: Boolean = true): this.type = and(name, multiple, (".*?"+name+".*").r)
    /** The `Parsley` instance associated with this stack-seeker.  Creates one if none exists yet. */
    def parsley: Parsley = synchronized {
      if (myParsley == null) { myParsley = new Parsley(sought.size, i => Option(sought(i-1)._1).getOrElse("Method "+i), Some(this)) }
      myParsley
    }
    /** Searches through the stack and updates counters */
    protected[Parsley] def gather() = {
      this.synchronized { building = false }
      val threads = threadMgr.dumpAllThreads(false,false)
      val counts = new Array[Int](sought.length)
      threads.foreach{ th =>
        val trace = th.getStackTrace.map(_.toString)
        for (i <- sought.indices) {
          var j = trace.length-1
          val patterns = sought(i)._3
          for (k <- patterns.indices.reverse) {
            while (j>=0 && patterns(k).unapplySeq(trace(j)).isEmpty) j -= 1
          }
          if (j >= 0) counts(i) += 1
          if (sought(i)._2) {
            j -= 1
            while (j >= 0) {
              if (patterns(0).unapplySeq(trace(j)).isDefined) counts(i) += 1
              j -= 1
            }
          }
        }
      }
      counts
    }
  }
  
  /** This class holds information about a set of alternative methods and lets you pick an implementation at random.  It may be associated with a particular Parsley instance. */
  class Callable[A,B] protected[Parsley] (op: Option[Parsley]) {
    private val rng = (new ichi.maths.Rng.Hybrid2).seedWithTime
    private var myParsley: Parsley = op.orNull
    private val calls = new collection.mutable.ArrayBuffer[(A => B)]
    private val names = new collection.mutable.ArrayBuffer[String]
    /** Add an additional named alternate method. */
    def add(name: String)(g: A => B): this.type = synchronized {
      calls += g
      names += name
      this
    }
    /** Add an additional un-named alternate method */
    def add(g: A => B): this.type = add(null: String)(g)
    def parsley: Parsley = synchronized {
      if (myParsley == null) { myParsley = new Parsley(calls.size, i => Option(names(i-1)).getOrElse("Method "+i)) }
      myParsley
    }
    /** Run one of the alternate methods, with the alternate chosen at random. */
    def run(a: A): B = {
      val (c,i) = synchronized {
        if (myParsley == null) throw new NoSuchElementException("Callable has no parsley.  (This does actually make sense.  Trust me.)")
        val i = rng.roll(calls.size)
        (calls(i), i)
      }
      myParsley.call[A,B](i+1)(a)(c)
    }
  }
  
  /** A utility method to define a set of alternate methods.  Usage: example: `methods[String,Int).add(_.length).add(_.toInt)` */
  def methods[A,B]: Parsley.Callable[A,B] = new Parsley.Callable[A,B](None)
  
  /** A utility method to define a stack search.  `name` is used only for labeling, `first` and `extra` must match for it to count. */
  def seek(name: String, multiple: Boolean, first: scala.util.matching.Regex, extra: scala.util.matching.Regex*) = new StackSeeker(name, multiple, first, extra: _*)
  /** A utility method to define a stack search, assuming multiple-counts for multiply-entered methods. */
  def seek(name: String, first: scala.util.matching.Regex, extra: scala.util.matching.Regex*) = new StackSeeker(name, true, first, extra: _*)
  /** A utility method to define a stack search to match a name (name is not escaped, so is actually a regex pattern). */
  def seek(name: String, multiple: Boolean = true) = new StackSeeker(name, multiple, (".*?"+name+".*").r)
}
