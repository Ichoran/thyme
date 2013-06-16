// ----------------------------------------------------------------------------
// Thyme is a lightweight microbenchmark assistant for Scala
// Copyright (c) 2013, Rex Kerr.  All rights reserved.
// Thyme is provided as open source software under the BSD 3-clause license
// See http://www.opensource.org/licenses/bsd-license.php for text
// ----------------------------------------------------------------------------



package ichi.bench

import scala.language.implicitConversions

import java.lang.management._
import scala.annotation.tailrec
import scala.math._
import ichi.maths._

/**
 * Thyme is a Timing Helper You Might Enjoy
 * 
 * Thyme provides a set of capabilities to help with microbenchmarking,
 * especially interactively and/or during development.  Thyme provides
 * benchmarking methods of four classes, depending on the sophistication
 * one requires.
 * 
 * Every test in Thyme expects a return value.  This is not an accident!
 * The JIT compiler will throw away your computations if it can figure out that
 * they don't matter.  Make them matter by returning a value that depends upon
 * all the work that's been done.
 * 
 * '''clock''' and related functions simply measure wall-clock time
 * (using Java's `System.nanoTime`) and otherwise get out of your way.
 * 
 * '''time''' and related functions monitor the status of the JVM while
 * performing timing, which allows (relatively) correct reporting of timing
 * even while garbage collection is being performed.  The
 * JVM should use a stop-the-world garbage collector for this to work well;
 * accuracy may suffer if a concurrent garbage collector is used, as the GC
 * time will mostly not have added to the runtime of the timed method.
 * 
 * '''bench''' and related functions perform a microbenchmark by repeatedly
 * calling code you supply, automatically performing sensible warm-ups,
 * reporting statistics, and performing the minimal computation required to
 * meet a desired accuracy level.
 * 
 * '''order''' and related functions perform a microbenchmark that seeks
 * the leading (possibly logarithmic) term in the algorithm's computational
 * complexity, returning x in O(n^x^) along with an error estimate.  These
 * routines are experimental.  (Even more experimental than the rest of the
 * library.)
 * 
 * Each class of benchmarking also (incidentally) returns most of the information
 * present in the previous level.
 * 
 * Thyme by default is re-entrant and thread-safe.  Lower level methods
 * take classes to hold timing data for performance reasons (leaving the
 * user to handle concurrency).  Higher-level methods create storage classes
 * as needed.
 * 
 * Thyme is ''not'' intended for automated performance regression testing or for
 * graphing performance; if you wish to use it for these things, you will need
 * to supply your own statistics and/or graphing libraries, and you may then
 * perfer a framework that launches multiple JVMs and/or uses classloaders to
 * force repeated loading/JIT compilation.
 * 
 * Thyme ''is'' intended for testing performance in-place in running code, for
 * testing performance tweaks in the REPL, and other common and quick
 * benchmarking tasks where you just want the right thing to happen with a
 * minimum of pomp and circumstance.
 * 
 * @define COMBO
 * Strategy for combining answers from repeated calls.  Default is to take the first or second depending on `unpredictablyFirst`.
 * 
 * @define RET_V
 * One of the answers computed by `f` as selected by `g`
 * 
 * @define EFFRT
 * Number of operations intrinsic to each call to `f` (default 1); this is used only to divide the reported time-per-operation.
 * 
 * @define EHHRT
 * Number of operations intrinsic to each call to `h` (default 1); this is used only to divide the reported time-per-operation.
 */
class Thyme(val accuracyTarget: Double = 0.03, watchLoads: Boolean = true, watchGarbage: Boolean = true, watchMemory: Boolean = true) { self =>
  private[this] val setupStartTime = System.nanoTime

  // ------------------------------------------------------------
  // Options for what is monitored and how output is delivered
  // ------------------------------------------------------------
  
  /** Sets the number of significant digits to print, which will be used to truncate decimal fractions.
   * Negative values mean to use SI prefixes when appropriate (ms, us, ns) instead of seconds.
   * Zero means print all digits out to the accuracy of the measurement (with units of seconds).
   * Default is -4, yielding outputs like 1.724 s or 41.93 ms.  Valid values are -18 to 18.
   */
  var printingPrecision: Int = -4
  
  /** Pretty-printer routine that will choose significant figures and units appropriately for
   * the value chosen (t).  Returns a string containing the value and appropriate units.
   */
  def printWithPrecision(t: Double, rawPrecision: Int = 9) = Thyme.printWithPrecision(t, printingPrecision, rawPrecision)
  
  /** The maximum number of classes that are allowed to be loaded before a timing is considered corrupted by class loading time.  Default is 1. */
  var allowedClassLoads: Int = 1
  
  /** Report whether class loading events are monitored. */
  def watchingLoads = watchLoads
  
  /** Whether garbage collection events are monitored. */
  def watchingGarbage = watchGarbage || watchMemory
  
  /** Whether memory usage is monitored (if yes, garbage is turned on also) */
  def watchingMemory = watchMemory
  
  /** Creates a mutable JvmStatus instance that obeys the watch flags */
  def createJS: JvmStatusMut = (new JvmStatusMut).enableCL(watchingLoads).enableGC(watchingGarbage).enableMem(watchingMemory)
  


  // ------------------------------------------------------------
  // "Clock" handling--simple elapsed wall clock time
  // ------------------------------------------------------------
  
  /** Times `n` sequential calls to `f`, combining the results with `g`, and returning the result
   * (to avoid optimizations that skip the computation entirely).  Result (in ns) passed to `elapsed`. 
   * The function is specialized on `Long` to enable primitives to be generated and computed without boxing.
   * (Other primitives must be packed into a `Long`, e.g. with `doubleToRawLongBits`.)
   */
  def clockMany[@specialized(Long) A](f: => A)(n: Int, op: (A,A) => A)(elapsed: Long => Unit): A = {
    var i = n-1
    val t0 = System.nanoTime
    var ans = f
    while (i > 0) {
      i -= 1
      ans = op(ans,f)
    }
    elapsed(System.nanoTime - t0)
    ans
  }
  
  /** Times the execution of the code block `f`.  Result passed to `elapsed` (in nanoseconds). 
   * The function is specialized on `Long` to enable primitives to be generated and computed without boxing.
   * (Other primitives must be packed into a `Long`, e.g. with `doubleToRawLongBits`.)
   */
  def clock[@specialized(Long) A](f: => A)(elapsed: Long => Unit): A = clockMany(f)(1, null)(elapsed)
  
  /** Times the execution of the code block `f` and returns both it and the elapsed time in nanoseconds */
  def clockPair[A](f: => A): (A, Double) = { var dt = 0L; val ans = clockMany(f)(1, null)(dt = _); (ans, dt*1e-9) }
  
  /** Times the execution of code block `f` and prints the elapsed time. */
  def pclock[A](f: => A, pr: String => Unit = Thyme.printer): A = {
    var dt = 0L
    val ans = clock(f)(dt = _)
    pr("Wall clock time elapsed: " + printWithPrecision(dt*1e-9))
    ans
  }
  
  /** Times the execution of code block `f` (which is assumed to contain `m` computations) `n` times in a row, and prints the time taken for a single computation. */
  def pclockN[A](f: => A)(m: Int, n: Int, op: (A,A) => A, pr: String => Unit = Thyme.printer): A = {
    var dt = 0L
    val ans = clockMany(f)(n, op)(dt = _)
    val N = (1 max m)*(1L max n)
    pr("Wall clock time each: " + Thyme.printWithPrecision(dt*1e-9/N, -(11 min 9+log10(N).toInt)))
    ans
  }



  // ------------------------------------------------------------
  // "Time" handling--single timed runs, JVM-state-aware
  // ------------------------------------------------------------
  
  /** Estimate runtime by subtracting garbage collection time from total time. */
  def runtimeEst(js: JvmStatus) = if (js.checkGC && js.gcCount > 0) 1e-9*js.nsTicks - 1e-3*js.gcTime else 1e-9*js.nsTicks
  
  /** Detect whether expected measurement error is within desired bounds */
  def accurate(js: JvmStatus, errorBound: Double = accuracyTarget) = {
    if (!js.isDiff || !js.checkT || (js.checkCL && js.clLoads > allowedClassLoads) || (1e-9*(js.nsTocks - js.nsTicks) > measurementUpperBound)) false
    else {
      val dt = runtimeEst(js)
      val ee = measurementIncrement + (if (js.gcCount>0 || js.clLoads>0) 1e-3 else 0)
      abs(ee/dt) <= abs(errorBound)
    }
  }
  
  /** Returns the estimate of the runtime or NaN if the estimate is expected to be inaccurate. */
  def runtimeOrNaN(js: JvmStatus) = if (!js.isDiff && !accurate(js)) Double.NaN else runtimeEst(js)
  
  /** Returns the estimate of the memory used if there's any hope it's accurate.  (It still may not be--memory use is poorly tracked.) */
  def memoryOrNaN(js: JvmStatus) = if (js.isDiff && js.checkMem && js.checkGC && js.gcCount==0) js.memUse else Double.NaN
  
  /** Returns the estimate of the time spent in garbage collection if this information is available. */
  def garbagetimeOrNaN(js: JvmStatus) = if (js.isDiff && js.checkGC) 1e-3*js.gcTime else Double.NaN
  
  /** Packages all the JVM status changes in `js` into a [[Thyme.Report]], with optional title and number-of-iterations. */
  def report(js: JvmStatusMut, title: String = "", N: Long = 0) = {
    new Thyme.Report(
      js.nsTocks*1e-9,
      if (js.checkCL) Some(js.clLoads) else None,
      if (js.checkGC) Some((js.gcTime*1e-3, js.gcCount)) else None,
      if (js.checkMem && js.checkGC && js.gcCount == 0) Some(js.memUse) else None,
      1e-9*(js.nsTocks-js.nsTicks),
      runtimeEst(js),
      !accurate(js),
      if (title.length > 0) Some(title) else None,
      if (N > 0) Some(N) else None,
      printingPrecision
    )
  }
  
  
  /** Times `n` sequential calls to `f`, combining the results with `g`, using `temp` as temporary storage and returning results in `elapsed`.
   * The function is specialized on `Long` to enable primitives to be generated and computed without boxing.  (Other primitives must be packed into a `Long`, e.g. with `doubleToRawLongBits`.)
   */
  def timeMany[@specialized(Long) A](f: => A)(n: Int, op: ((A,A) => A))(temp: JvmStatusMut, elapsed: JvmStatusMut): A = {
    var i = n-1
    temp.current
    var ans = f
    while (i > 0) {
      i -= 1
      ans = op(ans, f)
    }
    elapsed.current.diff(temp)
    ans
  }
  
  /** Times the execution of the code block `f`.  `temp` is used as temporary storage, and results are placed in `elapsed`.
   * Not reentrant or thread-safe.
   * The function is specialized on `Long` to enable primitives to be generated and computed without boxing.  (Other primitives must be packed into a `Long`, e.g. with `doubleToRawLongBits`.)
   */
  def time[@specialized(Long) A](f: => A)(temp: JvmStatusMut, elapsed: JvmStatusMut): A = timeMany(f)(1, null)(temp, elapsed)
  
  /** Times the execution of code block `f` and returns both the result of `f` and a timing and status report.
   */
  def timePair[A](f: => A): (A, Thyme.Report) = { val t,el = createJS; val ans = time(f)(t,el); (ans, report(el)) }
  
  /** Times the execution of code block `f` and prints the elapsed time and any other status information enabled in this class.
   * Will print warnings if there are indications of irregular timings or accuracy targets may not be met.
   */
  def ptime[A](f: => A, pr: String => Unit = Thyme.printer): A = {
    val t,el = createJS
    val ans = time(f)(t, el)
    pr(report(el).toString)
    ans
  }
  
  /** Times `n` executions of code block `f` (which is assumed to contain `m` computations), combining results with `g`;
   * prints the elapsed time per computation plus any other status information enabled in this class.
   * An optional `title` may be given.
   * Will print warnings if there are indications of irregular timings or accuracy targets may not be met.  Reentrant and thread-safe.
   */
  def ptimeN[A](f: => A)(m: Int, n: Int, op: (A,A) => A, title: String = "", pr: String => Unit = Thyme.printer): A = {
    val t,el = createJS
    val ans = timeMany(f)(n, op)(t, el)
    val N = (1 max m)*(1L max n)
    pr(report(el, title, N).toString)
    ans
  }
  
  
  
  // ------------------------------------------------------------
  // "Bench" handling--repeated timed runs for microbenchmarking
  // ------------------------------------------------------------
  
  /** This variable is used internally to thwart JVM optimization strategies that might confound microbenchmarking results */
  @volatile var unpredictablyFirst = false
  private val genericPicker: ((Any, Any) => Any) = if (unpredictablyFirst) _ else _
  
  /**
   * This method returns an Function2 that will generically pick one of the two variables on the basis of the `unpredictablyFirst` variable.
   */
  def uncertainPicker[A] = genericPicker.asInstanceOf[(A,A) => A]

  /** The target time, in seconds, for a benchmarking run (should be long enough to ensure JIT compilation by 50% completion) */
  var targetTime = 0.05
  /** Time limit, in seconds, for a benchmarking run; if it's been longer than this, just return the best result we have so far */
  var tooMuchTime = 5.0
  /** The number of iterations that surely ought to be enough to warm up even the simplest method (JVM-dependent; default is 200,000 which should be conservative). */
  var iterationsToSurelyWarm = 200000
  
  /** Tries to find the minimum number of runs of code block `f` that will take at least time `t`.  `g` is used to combine multiple runs.  Elapsed time of
   * final run ends up in `elapsed`; the answer is passed as a parameter to `targetN`.
   * @param f The code block to analyze.
   * @param t Target time
   * @param temp Storage space for initial timepoint; contents are not defined upon return
   * @param elapsed Storage space for the final timepoint and elapsed time.  Will contain the elapsed time of the last run upon return.
   * @param targetN Callback for when the number of runs is found--this is how the answer is communicated to the calling code.
   * @param op $COMBO
   * @param n0 Initial number of iterations to try; default is 1
   * @param mult Strategy for incrementing number of iterations to try; default is multiply by two
   * @param iter Number of tries to get a clean read at a given number of iterations; otherwise increment even if the time target may have been met.  Default is 4.
   * @return $RET_V
   */
  final def findTimeTarget[A](f: => A)(
    t: Double, temp: JvmStatusMut, elapsed: JvmStatusMut, targetN: Int => Unit,
    op: ((A,A) => A) = uncertainPicker[A],
    n0: Int = 1, mult: Int => Int = _*2, iter: Int = 4
  ): A = {
    var ans = timeMany(f)(n0, op)(temp, elapsed)
    var rt = runtimeEst(elapsed)
    var acc = accurate(elapsed)
    var ni = n0
    var i = 1 max iter
    while (rt < t || !acc) {
      if (!acc && i>1) i -= 1
      else if (n0 == Int.MaxValue) { targetN(n0); return ans }
      else {
        val n = mult(ni)
        ni = (if (n > ni) n else (ni*2L min Int.MaxValue).toInt)
        i = iter
        ans = op(ans, timeMany(f)(ni, op)(temp, elapsed))
        rt = runtimeEst(elapsed)
        acc = accurate(elapsed)
      }
    }
    targetN(ni)
    ans
  }
  
  /**
   * A class to hold a warmed-up piece of code.  On each call, it will be run `reps` times and the results will be combined with `op`.
   */
  class Warm[A] private (f: => A, val combine: ((A,A) => A), val reps: Int) { 
    def apply() = {
      var a = f
      var i = 1
      while (i < reps) {
        a = combine(a, f)
        i += 1
      }
      a
    }
  }
  /**
   * A builder for [[Warm]] classes to make sure that the code is actually warm.
   */
  object Warm {
    /**
     * Generates a `Warm` container for code in `f`.  Results will be combined with `op` (default is to take first or second depending on `unpredictablyFirst`).
     */
    def apply[A](f: => A, op: ((A, A) => A) = uncertainPicker[A]) = {
      val ja, jb = new JvmStatusMut
      var n = 1
      var a = findTimeTarget(f)(targetTime, ja, jb, n = _)    // We're finding the target time but we might not be warmed up!
      def getWarm = new Warm(f, op, max(1, n/4))
      val warm = getWarm
      val timings = new Array[Double](12)
      val indices = Array.tabulate(timings.length)(_.toDouble)
      var i = 0
      var plenty = 0
      while (i < timings.length) {
        plenty += 1
        val aa = time(warm())(ja, jb)
        a = op(a, aa)
        timings(i) = runtimeEst(jb)
        if (!jb.checkCL || jb.clLoads==0 || plenty > 10*timings.length) i += 1
      }
      def stable: Boolean = {
        val asd = new AccumulateSD
        var i = 0
        while (i < timings.length) { asd +~ timings(i); i += 1 }
        i = 0
        while (i < timings.length) {
          asd -~ timings(i)
          val diff = timings(i) - asd.mean
          if (diff > asd.mean*accuracyTarget && (asd.sem ==0 || asd.p(timings(i)) < 0.002)) return false
          asd +~ timings(i)
          i += 1
        }
        val bsd = new AccumulateSD
        bsd +~ timings(0)
        asd -~ timings(0)
        i = 1
        while (i < timings.length - 2) {
          bsd +~ timings(i)
          asd -~ timings(i)
          val diff = bsd.mean - asd.mean
          if (diff > (asd.mean+bsd.mean)*0.5*accuracyTarget) {
            if (asd.n > 3 && (asd.sem == 0 || asd.meanP(bsd.mean) < 0.002)) return false
            if (bsd.n > 3 && (bsd.sem == 0 || bsd.meanP(asd.mean) < 0.002)) return false
          }
          i += 1
        }
        Nonparametric.theilsen(indices,timings)()._1.meanTailP(0) > 0.002
      }
      while (plenty < 100 && !stable) {
        val slide = 3
        i = 0
        while (i+slide < timings.length) {
          timings(i) = timings(i+slide)
          i += 1
        }
        while (i < timings.length) {
          do {
            plenty += 1
            a = op(a, time(warm())(ja, jb))
            timings(i) = runtimeEst(jb)
          } while (jb.checkCL && jb.clLoads > 0 && plenty < 10*timings.length)
          i += 1
        }
      }
      a = findTimeTarget(f)(targetTime, ja, jb, n = _)    // We are presumably warm now, so if we find the target time we should be correct
      getWarm
    }
  }
  /** Import `autoWamer` to use the Warm variants almost transparently (note: semantics of by-name parameters prevents them from being used here). */
  implicit def autoWarmer[A](f: () => A): Warm[A] = Warm(f())
        
      
  
  /** Benchmarks code block `f`, storing results in `br` and combining results with `g`. 
   * @param f The code block to benchmark.
   * @param br A place to store results.
   * @param op $COMBO
   * @param sampleSize Minimum number of times to run the benchmark (for averaging, default 20)
   * @param minRepeats Number of times to repeat `f` within one benchmark (default 1)
   * @param effort $EFFRT
   * @param knownWarm Skip warmup?  (default: false)
   * @param targetTimeOverride Override the default target time for this benchmark
   * @return $RET_V
   */
  def bench[A](f: => A)(
    br: Thyme.Benched,
    op: ((A,A) => A) = uncertainPicker[A],
    sampleSize: Int = 20, minRepeats: Int = 1, effort: Int = 1, knownWarm: Boolean = false, targetTimeOverride: Double = Double.NaN
  ): A = {
    val t0 = System.nanoTime
    val targTime = if (targetTimeOverride.finite) targetTimeOverride else targetTime
    var repeats = max(1, minRepeats)
    val frpt = () => {
      var a = f
      var i = 1
      while (i < repeats) {
        a = op(a,f)
        i += 1
      }
      br.functionCalls += repeats
      a
    }
    br.mimic(Thyme.Benched.zero)
    val ns = 4 max sampleSize
    val temp, elapsed = createJS
    val trun, tmem, oldtmem = new Array[Double](ns)
    var nrun, ngar, nmem = 0
    var tgar = 0.0
    var cgar = 0
    var oldnmem = -1
    var rrun, rgar, rmem = repeats
    var warm = knownWarm
    var i = 0
    var rdst = Distribution.zero[Double]
    var biggered = 0
    var totalRuntime = 0.0
    var fixedErr = measurementIncrement
    def update() = {
      if (rrun == repeats) {
        totalRuntime += runtimeEst(elapsed)
        val r = runtimeOrNaN(elapsed)
        trun(i) = r
        if (!isNaN(r) && r > 0) nrun += 1
      }
      if (rgar == repeats) {
        tgar += (if (elapsed.checkGC && elapsed.gcCount > 0) { ngar += 1; elapsed.gcTime*1e-3 } else 0.0)
        cgar += elapsed.gcCount.toInt
      }
      if (rmem == repeats) {
        val m = memoryOrNaN(elapsed)
        tmem(i) = m
        if (i > 0 && m > 0 && tmem(i-1) > 0) nmem += 1
      }
      i += 1
    }
    var a = time(frpt())(temp, elapsed)
    update()
    do {
      while (i < sampleSize) {
        a = op(a, time(frpt())(temp, elapsed))
        update()
      }
      if (nmem >= oldnmem) {
        System.arraycopy(tmem, 0, oldtmem, 0, ns)
        nmem = oldnmem
        rmem = repeats
      }
      repeats = min(repeats*2L, Int.MaxValue).toInt
      fixedErr = measurementIncrement + max(0, 1e-3*((ngar+nrun)/sampleSize - 1))
      if (nrun >= sampleSize/2) rdst = Distribution.compute(trun, true, false, false, intrinsicError = fixedErr).dropOutliers
      if (!warm && (totalRuntime >= targTime || sampleSize*repeats >= iterationsToSurelyWarm)) warm = true
      if (biggered < 3 && (nrun < sampleSize/2 || (rdst.sem+fixedErr) > rdst.mean*accuracyTarget || !warm) && repeats < Int.MaxValue) {
        nrun = 0; rrun = repeats
        nmem = 0; rmem = repeats
        ngar = 0; cgar = 0; tgar = 0.0; rgar = repeats
      }
      if (rrun == repeats && nrun >= sampleSize/2) {
        biggered += 1
      }
      if (biggered > 2 && sampleSize*2L < Int.MaxValue) {
        bench(f)(br, op, sampleSize*2, repeats/2, effort, warm, targTime)
        warm = true
      }
      if (rrun == repeats) { nrun = 0; totalRuntime = 0 }
      i = 0
    } while (biggered < 3 && repeats < Int.MaxValue && rrun==repeats)
    if (br.runtimeResults.n < 1 || (rdst.n >= 1 && br.runtimeResults.sem*rdst.mean > rdst.sem*br.runtimeResults.mean)) {
      br.runtimeIterations = rrun
      br.runtimeResults = rdst
      br.intrinsicError = fixedErr
    }
    if (cgar > br.garbageCountsTotal) { br.garbageIterations = rgar; br.garbageCountsTotal = cgar; br.garbageTimePerReplicate = tgar/sampleSize }
    if (br.memoryResults.n < nmem) {
      val mem = new Array[Double](nmem)
      i = 1
      var j = 0
      while (j < mem.length) {
        if (tmem(i) > 0 && tmem(i-1) > 0) { mem(j) = tmem(i); j += 1 }
        i += 1
      }
      br.memoryIterations = rmem
      br.memoryResults = Distribution.compute(mem, true, false, false, 0)
    }
    if (br.runtimeResults.n > 0) { br.runtimeResults = Distribution.compute(br.runtimeResults.data.value, intrinsicError = br.intrinsicError) }
    val t1 = System.nanoTime
    br.effort = max(1, effort)
    br.wallClockTime = 1e-9*(t1-t0)
    a
  }
  
  /** Benchmarks the warmed-up code in `w`; see [[bench]] for details of how. */
  def benchWarm[A](w: Warm[A])(br: Thyme.Benched, sampleSize: Int = 20, minRepeats: Int = 1, effort: Int = 1, targetTimeOverride: Double = Double.NaN) = {
    bench(w())(br, w.combine, sampleSize, minRepeats, effort*w.reps, true, targetTimeOverride)
  }
  
  /** Benchmarks `f` and returns the result and a benchmarking report.  If `f` runs more than one iteration, the number of iterations can be specified with `effort`. */
  def benchPair[A](f: => A, effort: Int = 1, targetTime: Double = Double.NaN) = {
    val br = Thyme.Benched.empty
    br.printingPrecision = printingPrecision
    (bench(f)(br, effort = effort, targetTimeOverride = targetTime), br)
  }
  
  /** Benchmarks the warmed up code in `w`.  See [[benchPair]] for details of how. */
  def benchPairWarm[A](w: Warm[A], effort: Int = 1, targetTime: Double = Double.NaN) = benchPair(w(), effort*w.reps, targetTime)
  
  /** Benchmarks `f`, prints a report, and returns the result.
   * @param f The code to benchmark
   * @param effort $EFFRT
   * @param title If nonempty, will print on the first line
   * @param pr Place to send the output (default is println)
   * @return $RET_V
   */
  def pbench[A](f: => A, effort: Int = 1, title: String = "", pr: String => Unit = Thyme.printer) = {
    val br = Thyme.Benched.empty
    br.title = title
    val ans = bench(f)(br, effort = effort)
    pr(br.toString)
    ans
  }
  
  /** Benchmarks warmed up code and prints the result.  See [[pbench]] for details of how. */
  def pbenchWarm[A](w: Warm[A], effort: Int = 1, title: String = "", pr: String => Unit = Thyme.printer) = pbench(w(), effort*w.reps, title, pr)
  
  
  /** Benchmarks `f` and `h` against each other, setting the results in `bf`, and returning a computed value.
   * The benchmark randomly shuffles between calls to `f` and `h` and analyzes the time taken as a function of
   * the proportion of calls to `f` vs `h`.
   * @param f The first code block to benchmark
   * @param feffort $EFFRT
   * @param h The second code block to benchmark
   * @param heffort $EHHRT
   * @param bf The data structure to store the results
   * @param op $COMBO
   * @param sampleSize Minimum number of different mixtures of `f` and `h` (for averaging, default 20)
   * @param callsPerMix Minimum number of times to call either `f` or `h` per mixture (default 10)
   * @param oneToOne Makes sure `f` and `h` are called the same number of times even if one is dramatically faster than the other (default: false)
   * @param knownWarm Skip warmup?  (default: false)
   * @param targetTimeOverride Override the default target time for a single benchmark call
   * @param seed A seed for the random number generator used to pick order of calls to `f` and `h`.  Use `System.nanoTime` if you want it different each time
   * @return $RET_V 
   */
  def benchOff[A](f: => A, feffort: Int = 1)(h: => A, heffort: Int = 1)(
    bf: Thyme.Comparison,
    op: ((A,A) => A) = uncertainPicker[A],
    sampleSize: Int = 20, callsPerMix: Int = 10, oneToOne: Boolean = false, knownWarm: Boolean = false,
    targetTimeOverride: Double = Double.NaN, seed: Long = Thyme.defaultSeed
  ): A  = {
    val t0 = System.nanoTime
    val rng = Rng.Hybrid2().seedL(seed)
    var a =  bench(f)(bf.br1, op, sampleSize, 1, feffort, knownWarm, targetTimeOverride)
    a = op(a,bench(h)(bf.br2, op, sampleSize, 1, heffort, knownWarm, targetTimeOverride))
    bf.oneToOne = oneToOne
    val (nf,nh) = if (oneToOne) (1,1) else (max(1,(1+bf.br1.runtimeIterations)/2).toInt, max(1,(1+bf.br2.runtimeIterations)/2).toInt)
    val reps = if (oneToOne) max(1, max(callsPerMix, sqrt(bf.br1.runtimeIterations*bf.br2.runtimeIterations).toInt)) else max(1,callsPerMix)
    bf.mixSizes = (nf, nh, reps)
    val data = Thyme.ComparedData(sampleSize)
    val who = new Array[Byte](reps)
    val temp, bs = new JvmStatusMut
    for (index <- data.mixT.indices) {
      val i = data.mixT.length/2 + index/2 - (index&0x1)*index
      val fr = (i+1).toDouble/(data.mixT.length+1)
      var j = round(who.length*fr).toInt
      var k = who.length-j
      var l = 0
      data.weight(i) = (k-j).toDouble/(j+k)
      while (j+k > 0) {
        who(l) = (if (j.toFloat >= rng.nextFloat*(j+k)) { j -= 1; 0 } else { k -= 1; 1 })
        l += 1
      }
      l = who.length-1
      while (l > 0) {
        if (who(l)!=who(l-1)) data.swaps(i) += 1
        l -= 1
      }
      temp.current
      l = 0
      while (l < who.length) {
        if (who(l)==0) {
          var m = 0
          while (m < nf) {
            a = op(a,f)
            m += 1
          }
        }
        else {
          var m = 0
          while (m < nh) {
            a = op(a,h)
            m += 1
          }
        }
        l += 1
      }
      bs.current diff temp
      data.mixT(i) = runtimeEst(bs)
      data.mixM(i) = memoryOrNaN(bs)
      if (isNaN(data.mixM(i))) data.nbadM += 1
      data.mixG(i) = garbagetimeOrNaN(bs)
      if (isNaN(data.mixG(i))) data.nbadG += 1
    }
    bf.data = Some(data)
    bf.intrinsicError = measurementIncrement/sqrt(data.mixT.length) + 1e-3*sqrt(data.mixG.length-data.nbadG)/data.mixG.length
    def fitTMG(x: Array[Double], fix: Option[(Array[Double],Thyme.ComparedResult)]): Thyme.ComparedResult = {
      def ts(y0: Array[Double], fix: Option[(Array[Double],Thyme.LineFit)]) = {
        val y = fix match {
          case None => y0
          case Some((xf, lf)) =>
            val yf = new Array[Double](y0.length)
            var i = 0
            while (i < yf.length) {
              yf(i) = y0(i) - lf.y(xf(i))
              i += 1
            }
            yf
        }
        Thyme.LineFit(x,y)
      }
      new Thyme.ComparedResult(
        ts(data.mixT, fix.map(a=>a.copy(_2=a._2.run))),
        ts(data.mixG, fix.map(a=>a.copy(_2=a._2.gct)))
      )
    }
    bf.wtfit = fitTMG(data.weight, None)
    bf.swapfit = fitTMG(data.swaps, Some((data.weight, bf.wtfit)))
    val t1 = System.nanoTime
    bf.wallClockTime = 1e-9*(t1-t0)
    if (bf.runtimeAccuracy > accuracyTarget && bf.wallClockTime < tooMuchTime) {
      val wct = bf.wallClockTime
      a = benchOff(f, feffort)(h, heffort)(bf, op, min(sampleSize*3L/2,Int.MaxValue).toInt, min(callsPerMix*3L/2,Int.MaxValue).toInt, oneToOne, knownWarm, targetTimeOverride, rng.stateL)
      bf.wallClockTime += wct
    }
    a 
  }
  
  /** Compares two warmed up pieces of code, `w` and `v`.  See [[benchOff]] for details of how. */
  def benchOffWarm[A](w: Warm[A], weffort: Int = 1)(v: Warm[A], veffort: Int = 1)(
    bf: Thyme.Comparison, sampleSize: Int = 20, callsPerMix: Int = 10, targetTimeOverride: Double = Double.NaN, seed: Long = Thyme.defaultSeed
  ) = {
    benchOff(w(), weffort*w.reps)(v(), veffort*v.reps)(bf, w.combine, sampleSize, callsPerMix, false, false, targetTimeOverride, seed)
  }
  
  /** Benchmarks `f` against `h` and returns both the result and a benchmark report with an optional `title`. 
   * See `benchOff` for more details regarding parameters.
   */
  def benchOffPair[A](
    title: String = "", targetTime: Double = Double.NaN, op: (A,A) => A = uncertainPicker[A]
  )(
    f: => A, feffort: Int = 1, ftitle: String = ""
  )(
    h: => A, heffort: Int = 1, htitle: String = ""
  ) = {
    val bf = Thyme.Comparison.empty
    bf.br1.title = ftitle
    bf.br2.title = htitle
    bf.title = title
    bf.printingPrecision = printingPrecision
    val a = benchOff(f, feffort)(h, heffort)(bf, op, targetTimeOverride = targetTime)
    (a, bf)
  }
  
  /** Compares two warmed up pieces of code, `w` and `v`, and returns a report.  See [[benchOffPair]] for details of how. */
  def benchOffPairWarm[A](title: String = "", targetTime: Double = Double.NaN)(w: Warm[A], weffort: Int = 1, wtitle: String = "")(v: Warm[A], veffort: Int = 1, vtitle: String = "") = {
    benchOffPair(title, targetTime, w.combine)(w(), weffort*w.reps, wtitle)(v(), veffort*v.reps, vtitle)
  }
  
  /** Benchmarks `f` against `h` and prints the result (lines sent to `pr`, which is `println` by default).  See `pbench` for details of arguments. */
  def pbenchOff[A](
    title: String = "", pr: String => Unit = Thyme.printer, op: (A,A) => A = uncertainPicker[A]
  )(
    f: => A, feffort: Int = 1, ftitle: String = ""
  )(
    h: => A, heffort: Int = 1, htitle: String = ""
  ) = {
    val (a, bf) = benchOffPair(title, op = op)(f, feffort, ftitle)(h, heffort, htitle)
    pr(bf.toString)
    a
  }
  
  /** Benchmarks warmed up code `w` against `v` and prints the result; see [[pbenchOff]] for details of how. */
  def pbenchOffWarm[A](title: String = "", pr: String => Unit = Thyme.printer)(w: Warm[A], weffort: Int = 1, wtitle: String = "")(v: Warm[A], veffort: Int = 1, vtitle: String = "") = {
    pbenchOff(title, pr, w.combine)(w(), weffort*w.reps, wtitle)(v(), veffort*v.reps, vtitle)
  }
  
  
  // ----------------------------------------------------------------
  // "order" handling--O(n^x) computation with and without a log term
  // ----------------------------------------------------------------
  /** '''WARNING--this is experimental code and subject to change.'''
    * `order` estimates the computational complexity of provided code `f`.
    * @param resource Generates resources needed for `f` to run.  See source for details.
    * @param f The code to test, which should do `n`-much work (i.e. it should scale the operation according to the var `n` in the resource; `data` will be at least enough and may be more than enough).
    * @param bo An output record.
    * @param n0 The midpoint for the size of the computation.
    * @param g Combines two results from the function (default: picks based on a volatile var)
    * @param minVariation What factor larger/smaller should be tested, at a minimum (default: `sqrt(2)`)?
    * @param maxVariation Up to what factor larger/smaller can be tested if statistics are ambiguous (default: `4`)?
    * @param sampleSize The number of different sizes tested (default: `10`)
    * @param numPerSample The number of repeats at each size (default: `20`)
    * @param bootstrapN Deprecated.  Not used.
    * @param targetTimeOverride Often need to override the default target time to get this to finish in a reasonable amount of time (default: `Double.NaN`, meaning do not override)
    * @param seed A seed used for the random number generator.
    */
  def order[A,B](resource: Int => Thyme.Resource[B])(f: Thyme.Resource[B] => A)(
    bo: Thyme.Scaled,
    n0: Int,
    g: ((A,A) => A) = (a: A, aa: A) => if (unpredictablyFirst) a else aa,
    minVariation: Float = sqrt(2).toFloat,
    maxVariation: Float = 4,
    sampleSize: Int = 10,
    numPerSample: Int = 20,
    bootstrapN: Int = 100,
    targetTimeOverride: Double = Double.NaN,
    seed: Long = 0x36ac75e92b251b1fL
  ): A = {
    val t0 = System.nanoTime
    val lo = if (minVariation > 1) 1/minVariation else minVariation
    val hi = if (minVariation < 1) 1/minVariation else minVariation
    val LO = if (maxVariation > 1) 1/maxVariation else maxVariation
    val HI = if (maxVariation < 1) 1/maxVariation else maxVariation
    if (!(lo.finite && hi.finite && LO.finite && HI.finite && lo>0 && hi>0)) throw new IllegalArgumentException("Variation must be positive")
    if (round(n0*LO)<1) throw new IllegalArgumentException("Variation too large: might use zero elements")
    if (n0*HI >= Int.MaxValue) throw new IllegalArgumentException("Variation too large: might use more than Int.MaxValue elements")
    if (round(n0*HI)-round(n0*LO) < sampleSize) throw new IllegalArgumentException("Maximum variation will not fill desired sample size.")
    if (sampleSize < 4) throw new IllegalArgumentException("Sample size must be at least 4.")
    if (numPerSample < 6) throw new IllegalArgumentException("Number of measurements per sample must be at least 6.")
    if (bootstrapN < 10) throw new IllegalArgumentException("Must sample at least 10 times for bootstrap error estimation.")
    if (LO > lo) throw new IllegalArgumentException("Maximum variation less than minimum variation?!")
    
    if (bo.benches == null || bo.benches.length != sampleSize) bo.benches = Array.fill(sampleSize)(Thyme.Benched.empty)
    if (bo.samples == null || bo.samples.length != sampleSize) bo.samples = new Array[Int](sampleSize)
    if (bo.numbers == null || bo.numbers.length != sampleSize) bo.numbers = new Array[Int](sampleSize)
    bo.center = n0
    bo.fits = null
    val targTime = if (targetTimeOverride.finite) targetTimeOverride else targetTime*0.5
    
    val ns = bo.numbers
    var logstep = log(hi/lo)/(sampleSize-1)
    val maxlog = log(HI/LO)/(sampleSize-1)
    val rng = (new Rng.Hybrid2).seedL(seed)
    var r: Thyme.Resource[B] = null
    var a: A = null.asInstanceOf[A]
    var okay, maxed = false
    var iter, lastiter = 0
    while (!okay) {
      var i = 0
      while (i < sampleSize) {
        ns(i) = round(n0*exp(logstep*(i - (sampleSize-1)*0.5))).toInt
        i += 1
      }
      var distinct = true
      i = 1
      while (i < sampleSize && distinct) {
        distinct = distinct & (ns(i) != ns(i-1))
        i += 1
      }
      if (!distinct && maxed) {
        ns(0) = round(LO*n0).toInt
        ns(ns.length-1) = round(HI*n0).toInt
        i = 1
        while (i < ns.length-1) {
          ns(i) = round(n0*((HI-LO).toDouble*i/(sampleSize-1) + LO)).toInt
          i +=1 
        }
      }
      if (maxed) okay = true
      if (r==null || r.N < ns(ns.length-1)) r = resource(ns(ns.length-1))
      if (distinct || maxed) {
        if (iter == 0) {
          r.n = n0
          a = bench(f(r))(bo.benches(bo.benches.length/2), targetTimeOverride = targTime*2)
        }
        i = 0
        while (i < ns.length) {
          bench{
            r.start = if (ns(i)<ns.length) rng.roll(ns.length-ns(i)) else 0
            r.n = ns(i)
            f(r)
          }(bo.benches(i), sampleSize = numPerSample, targetTimeOverride = targTime)
          i += 1
        }
        iter += 1
      }
      logstep = logstep + 0.25*log(2)/(sampleSize-1)
      if (logstep > maxlog) {
        logstep = maxlog
        maxed = true
      }
      val logns, llogns = new Array[Double](ns.length)
      i = 0
      if (lastiter < iter) {
        lastiter = iter
        while (i < ns.length) { 
          logns(i) = log(ns(i))
          llogns(i) = log(logns(i))
          i += 1
        }
        i = 0
        var sampleN = 0
        while (i < sampleSize) {
          val x = bo.benches(i).runtimeResults.data.value.length
          bo.samples(i) = x
          sampleN += x
          i += 1
        }
        val xs, ys, zs = new Array[Double](sampleN)
        i = 0
        for (j <- bo.benches.indices) {
          val b = bo.benches(j)
          val effort = b.runtimeEffort
          val runs = b.runtimeResults.data.value
          var k = 0
          while (k < runs.length) {
            xs(i) = logns(j)
            ys(i) = log(runs(k)/effort)
            zs(i) = ys(i) - log(xs(i))
            i += 1
            k += 1
          }
        }
        bo.fits = Thyme.ScaleFit(Thyme.LineFit(xs, ys), Thyme.LineFit(xs, zs), sampleSize)(xs, ys)
      }
      okay = okay || (bo.fits != null && (bo.fits.used.slope.sem / max(abs(bo.fits.used.slope.mean),1) <= accuracyTarget))
    }
    val t1 = System.nanoTime
    bo.wallClockTime = 1e-9*(t1-t0)
    a
  }
  
  /** Order handling.  See `order` for details of parameters.  Returns a tuple containing a result from `f` and the benchmarking report. */
  def orderPair[A,B](resource: Int => Thyme.Resource[B])(f: Thyme.Resource[B] => A)(n0: Int, targetTime: Double = Double.NaN): (A, Thyme.Scaled) = {
    val bo = Thyme.Scaled.empty
    val ans = order(resource)(f)(bo, n0, targetTimeOverride = targetTime)
    (ans, bo)
  }
  
  /** Order handling.  See `order` for details of parameters.  Returns a tuple containing a result from `f`; prints a benchmarking report. */
  def porder[A,B](resource: Int => Thyme.Resource[B])(f: Thyme.Resource[B] => A)(n0: Int, title: String = "", pr: String => Unit = Thyme.printer): A = {
    val bo = Thyme.Scaled.empty
    val ans = order(resource)(f)(bo,n0)
    bo.title = title
    pr(bo.toString)
    ans
  }
  
  /** Order handling.  Packs functions into `Resource` automatically. (Args for `f` are `start`, `n`, `data`.) Returns a tuple containing a result from `f`; prints a benchmarking report. */
  def porder[A,B](resource: Int => B)(f: (Int,Int,B) => A)(n0: Int, title: String = "", pr: String => Unit = Thyme.printer): A = {
    val bo = Thyme.Scaled.empty
    val ans = order(Thyme.Resource(resource))(Thyme.Resource.inline(f))(bo, n0)
    bo.title = title
    pr(bo.toString)
    ans
  }

  
  // ------------------------------------------------------------
  // Values computed at runtime to help estimate error bounds
  // ------------------------------------------------------------
  
  /** An expected bound for the time taken by a System.nanoTime call */
  val measurementIncrement = (1 to 10).map{ _ =>
    val a = new Array[Long](10000)
    var i = 0
    while (i < a.length) { a(i) = System.nanoTime; i += 1 }
    i = a.length/2
    var nz = 0
    val as = new AccumulateSD
    while (i < a.length) { val x = a(i) - a(i-1); a(i-1) = x; if (x==0) { nz += 1 } else { as +~ x }; i += 1 }
    val t = as.x(0.999)*1e-9
    if (nz > as.n*4L || t > 1e-3) 1e-3 else {
      val b = new Array[Double](a.length/2)
      i = a.length/2
      var j = 0
      while (j < b.length) { b(j) = a(i)*1e-9; i += 1; j += 1 }
      var ds = Distribution.compute(b, true, false, false, 0)
      while (!ds.sub.dists.isEmpty) ds = ds.sub.dists.head._1
      ds.x(0.999)
    }
  }.drop(5).max
  
  /** An expected bound for the time taken to get full status information */
  val measurementUpperBound = (1 to 10).map{ _ =>
    val t,e = createJS
    val a = new Array[Double](1000)
    var i = 0
    while (i < a.length) {
      t.current
      i += 1
      e.current
      e diff t
      a(i-1) = (e.nsTocks - e.nsTicks)*1e-9
    }
    val b = new Array[Double](a.length/2)
    System.arraycopy(a,a.length/2,b,0,b.length)
    var ds = Distribution.compute(b, true, false, false, 0)
    while (!ds.sub.dists.isEmpty) ds = ds.sub.dists.head._1
    if (measurementIncrement == 1e-3) 1.5e-3 else min(1e-3, sqrt(1e-3*ds.x(0.999)))
  }.drop(5).max
  

  private[this] val setupStopTime = System.nanoTime
  
  /** How long (in seconds) it took to prepare and warm up this instance of the Thyme class */
  val setupElapsedTime = 1e-9*(setupStopTime - setupStartTime)
}

object Thyme {
  private final val NormalSigmasAt025 = 1.9599639845400542
  
  /** The default location for printing: println */
  final val printer = (s: String) => println(s)
  
  /** The default seed for methods that use random numbers. */
  final val defaultSeed = 0x36ac75e92b251b1fL
  
  /** Aligns multiple lines (separated by \n) to tabs (\t). */
  protected[Thyme] def tab(s: String) = {
    val lines = s.split("\n",-1)
    val tabs = lines.map(_.zipWithIndex.filter(_._1 == '\t').map(_._2))
    val stops = Iterator.from(1).map(i => tabs.filter(_.length >= i).map(_(i-1))).takeWhile(_.length > 0).map(_.max).toArray
    lines.map{ l => 
      val bits = l.split('\t')
      ((0,"") /: bits) { (c,s) =>
        val ss = c._2 + s
        val i = if (c._1+1 >= bits.length) ss.length else stops(c._1)
        (c._1+1, ss + (if (ss.length < i) " "*(i-ss.length) else ""))
      }._2.mkString
    }.mkString("\n")
  }
  
  /** Prints a duration with units of time, paying some mind to both SI units and the accuracy/precision of the measurement. */
  protected[Thyme] def printWithPrecision(t: Double, printingPrecision: Int, rawPrecision: Int = 9): String = {
    val nonzerochar = (c: Char) => c.isDigit && c != '0'
    def munch(s: String) = {
      val s1 = s.dropWhile(_ == '0')
      if (s1.length==0) s.take(1) else s1
    }
    val s = ("%."+rawPrecision+"f").format(t)    
    if (printingPrecision == 0) return s+" s"
    val prec = (abs(printingPrecision) max 1) min 18
    val (sign,nonsign) = s.span(c => !c.isDigit && c!='.')
    var leading = s.takeWhile(_ != '.')
    var trailing = s.dropWhile(_ != '.').drop(1)
    var shift = 0
    val nzIndex = {
      val j = leading.indexWhere(nonzerochar)
      if (j>=0) leading.length-j else {
        val i0 = trailing.indexWhere(nonzerochar)
        val i = if (i0 < 0) trailing.length else i0
        if (printingPrecision < 0 || i0<0) {
          if (i >= 6 && trailing.length>=9 && i+prec>=9) { leading = munch(trailing.take(9)); trailing = trailing.drop(9); shift = 9; 9-i }
          else if (i >= 3 && trailing.length>=6 && i+prec>=6) { leading = munch(trailing.take(6)); trailing = trailing.drop(6); shift = 6; 6-i }
          else if (trailing.length>=3 && i+prec>=3) { leading = munch(trailing.take(3)); trailing = trailing.drop(3); shift = 3; 3-i }
          else -i
        }
        else -i
      }
    }
    if (nzIndex+rawPrecision-shift > prec && prec > nzIndex) printWithPrecision(t, printingPrecision, shift+prec-nzIndex)
    else {
      val units = if (shift==9) " ns" else if (shift==6) " us" else if (shift==3) " ms" else " s"
      trailing = trailing.take(0 max (prec-nzIndex))
      if (t==0) "0" + units
      else if (trailing.length > 0 || prec >= nzIndex) sign + leading + "." + trailing + units
      else sign + leading + units
    }
  }
  
  /**
   * Contains a report on an elapsed time, including what may have happened with garbage collection,
   * memory usage (to the extent that the JVM allows, which is usually "not much"), and class loading.
   * Note that JIT compilation is usually done at least partially in parallel, so it cannot accurately
   * be included.
   */
  class Report(
    /** The total time spent running code, including class loads, GC, JIT compilation, etc. */
    val totalTime: Double,
    /** The number of classes loaded during the period, if available. */
    val classLoads: Option[Long],
    /** The time spent (in seconds) and number of sweeps of garbage collection, if available. */
    val garbage: Option[(Double,Long)],
    /** Memory used (in bytes), if available.  The JVM may not have reported accurately even if the value is available. */
    val memoryUsed: Option[Long],
    /** Total time of measurement. */
    val measurementTime: Double,
    /** Time spent running code. */
    val runTime: Double,
    /** True if the accuracy bounds do not appear to have been met. */
    val inaccurate: Boolean,
    /** An optional title for text printing. */
    val title: Option[String] = None,
    /** An optional intrinsic effort (e.g. number of iterations) represented by the run of code.  (`None` is equivalent to `1`, or a single run.) */
    val effort: Option[Long] = None,
    /** The desired printing precision. */
    val printingPrecision: Int = -4
  ) {
    /** Generate a new report with a different level of effort. */
    def reEffort(N: Long) = new Report(totalTime, classLoads, garbage, memoryUsed, measurementTime, runTime, inaccurate, title, if (N>0) Some(N) else None, printingPrecision)
    /** Generate a new report with a different title. */
    def reTitle(t: String) = new Report(totalTime, classLoads, garbage, memoryUsed, measurementTime, runTime, inaccurate, if (t.isEmpty) None else Some(t), effort, printingPrecision)
    /** Generate a new report with different precision for printing. */
    def rePrint(prec: Int) = new Report(totalTime, classLoads, garbage, memoryUsed, measurementTime, runTime, inaccurate, title, effort, prec)
    /** Multi-line output describing what happened: elapsed time, garbage collection, and so on. */
    override lazy val toString = {
      val pp = printingPrecision
      val sb = new StringBuilder
      val prec = garbage.filter(_._2 > 0).map(_ => 3).getOrElse(9)
      sb ++= "Elapsed time"+title.map(" for " + _).getOrElse("") + 
        effort.map(" N="+_).getOrElse("")+": " + 
        (if (inaccurate) "~"+printWithPrecision(runTime, pp, prec)+" (inaccurate)" else printWithPrecision(runTime, pp, prec))
      for (e <- effort if e > 1) { sb ++= ("\n  Time each: "+(if (inaccurate) "~" else "")+printWithPrecision(runTime/e, pp, 11 min prec+log10(e).toInt)) }
      if (inaccurate) sb ++= ("\n  Total time: "+printWithPrecision(totalTime, pp)+"\n  Measurement time: "+printWithPrecision(measurementTime,pp))
      garbage.foreach{ case (t,c) => sb ++= (if (c>0) "\n  Garbage collection ("+c+" sweeps) took: "+printWithPrecision(t, pp, 3) else "\n  No garbage collection.") }
      classLoads.foreach{ n => sb ++= ("\n  "+n+" classes were loaded") }
      memoryUsed.foreach{ m => sb ++= ("\n  Memory used"+effort.map(e=>" each: %.1f, total".format(m.toDouble/e)).getOrElse("")+": "+m) }
      sb.result
    }
  }
  
  /**
   * Contains a report on a benchmarking run.
   */
  class Benched(
    /** Number of times the code was run during the benchmark where a reasonable estimate of runtime was obtained. */
    var runtimeIterations: Long,
    /** The raw results of the final run, packed into a standard deviation and robust statistic calculation class. */
    var runtimeResults: Distribution[Double],
    /** The number of iterations where garbage collection was monitored. */
    var garbageIterations: Long,
    /** The total number of garbage colleciton sweeps. */
    var garbageCountsTotal: Long,
    /** How much time was spent in garbage collection per iteration. */
    var garbageTimePerReplicate: Double,
    /** The number of iterations where memory usage might have been monitored correctly. */
    var memoryIterations: Long,
    /** The memory usage (in bytes), packed into a robust statistics class. */
    var memoryResults: Distribution[Double],
    /** The timing error intrinsic to each measurement. */
    var intrinsicError: Double,
    /** The total number of times the benchmarking routine called the function being benchmarked. */
    var functionCalls: Long,
    /** The time in seconds taken for the entire benchmarking run. */
    var wallClockTime: Double,
    /** The intrinsic number of iterations per function call (default = 1). */
    var effort: Int = 1,
    /** The precision with which to print results (default: use SI units, keep 4 digits) */
    var printingPrecision: Int = -4,
    /** An optional title for printing. */
    var title: String = ""
  ) {
    /** Look the same as another Benched class `b` by changing all our vars */
    def mimic(b: Benched): this.type = {
      runtimeIterations = b.runtimeIterations
      runtimeResults = b.runtimeResults
      garbageIterations = b.garbageIterations
      garbageCountsTotal = b.garbageCountsTotal
      garbageTimePerReplicate = b.garbageTimePerReplicate
      memoryIterations = b.memoryIterations
      memoryResults = b.memoryResults
      intrinsicError = b.intrinsicError
      functionCalls = b.functionCalls
      wallClockTime = b.wallClockTime
      this
    }
    /** The total effort expended during runtime measurements (effort times iterations) */
    def runtimeEffort: Double = runtimeIterations * max(effort, 1)
    /** The average time spent per iteration (counting effort per function call) */
    def runtime: Double = if (runtimeIterations<=0 || runtimeResults.n==0) Double.NaN else runtimeResults.mean / runtimeEffort
    /** An estimate of the error in the `runtime` measurement. */
    def runtimeError: Double = if (runtimeIterations<=0 || runtimeResults.n<=1) Double.NaN else (runtimeResults.sem + intrinsicError)/ runtimeEffort
    /** An estimate of error plus intrinsic measurement inaccuracies in the `runtime` measurement. */
    def runtimeAccuracy: Double = if (runtimeIterations<=0 || runtimeResults.n<=1 || runtimeResults.mean==0) Double.PositiveInfinity else (runtimeResults.sem + intrinsicError) / runtimeResults.mean
    /** Returns a 95% confidence interval for the runtime taken per iteration (taking effort into account).  Don't take this too seriously--it assumes
     * Gaussian statistics which are often not true. */
    def runtimeCI95: (Double, Double) = {
      if (runtimeIterations<=0 || runtimeResults.n<=1) (Double.NegativeInfinity, Double.PositiveInfinity)
      else {
        val x025 = NormalSigmasAt025*(runtimeResults.sem + intrinsicError)
        val iri = 1.0/runtimeEffort
        ((runtimeResults.mean - x025)*iri, (runtimeResults.mean + x025)*iri)
      }
    }
    /** Total effort expended during measurements that read garbage collection. */
    def garbageEffort: Double = garbageIterations * max(effort, 1)
    /** Average time spent per iteration in garbage collection (counting effort per function call) */
    def garbage: Double = if (garbageIterations<=0 || garbageCountsTotal<=0) Double.NaN else garbageTimePerReplicate / garbageEffort
    /** Estimate of error in `garbage` measurement. */
    def garbageError: Double = if (garbageIterations<=0 || garbageCountsTotal<=0) Double.NaN else garbageTimePerReplicate / (garbageEffort * sqrt(garbageCountsTotal))
    /** Estimate of error in `garbage` measurement including intrinsic measurement problems. */
    def garbageAccuracy: Double = garbageError/garbage
    /** Total effort expended during measurements that may have yielded a useful memory usage. */
    def memoryEffort: Double = memoryIterations * max(effort, 1)
    /** Estimate of average memory used per iteration (counting effort per function call ). */
    def memory: Double = if (memoryIterations<=0 || memoryResults.n==0) Double.NaN else memoryResults.mean / memoryEffort
    /** Error in `memory` estimate */
    def memoryError: Double = if (memoryIterations<=0 || memoryResults.n<=1) Double.NaN else memoryResults.sem / memoryEffort
    /** Error in `memory` estimate taking intrinsic problems into account (but probably not well enough) */
    def memoryAccuracy: Double = if (memoryIterations<=0 || memoryResults.n<=1 || memoryResults.mean==0) Double.PositiveInfinity else memoryResults.sem / memoryResults.mean
    /** Confidence interval for `memory`.  This measurement is so unreliable that this method is probably useless, unfortunately. */
    def memoryCI95: (Double, Double) = {
      if (memoryIterations<=0 || memoryResults.n<=1) (Double.NegativeInfinity, Double.PositiveInfinity)
      else {
        val x025 = NormalSigmasAt025*memoryResults.sem
        val iri = 1.0/runtimeEffort
        ((memoryResults.mean - x025)*iri, (memoryResults.mean + x025)*iri)
      }
    }
    /** Multi-line text description of this benchmarking run. */
    override def toString = {
      val pp = printingPrecision
      val tprec = ceil(-log(max(intrinsicError,1e-9))).toInt
      val rprec = ceil(-log(max(runtimeError,1e-12))).toInt+1
      val gprec = ceil(-log(max(garbageError,1e-12))).toInt+1
      val (r025,r975) = runtimeCI95
      val (m025,m975) = memoryCI95
      s"Benchmark${(if (title.isEmpty) "" else " for "+title)} ($functionCalls calls in ${printWithPrecision(wallClockTime, pp, tprec)})" +
      s"\n  Time:    ${printWithPrecision(runtime, pp, rprec)}   95% CI ${printWithPrecision(r025, pp, rprec)} - ${printWithPrecision(r975, pp, rprec)}   (n=${runtimeResults.n})" +
      (if (!isNaN(memory)) "\n  Memory:  %.1f   95%% CI %.1f - %.1f  (n=%d)".format(memory, m025, m975, memoryResults.n) else "") +
      (if (garbageCountsTotal>0) s"\n  Garbage: ${printWithPrecision(garbage, pp, gprec)}   (n=$garbageCountsTotal sweeps measured)" else "")
    }
  }
  object Benched {
    /** The statistical distribution of no points */
    val EmptyDistribution = Distribution.zero[Double]
    /** An unloaded benchmarking report */
    def empty = new Benched(0, EmptyDistribution, 0, 0, 0.0, 0, EmptyDistribution, 1e-9, 0, 0.0)
    private[this] val ZeroBenched = empty
    protected[bench] def zero = ZeroBenched
  }
  
  /** Stores timing data for runtime, garbage, and memory usage for runs that mix two different function calls */
  case class ComparedData(mixT: Array[Double], mixG: Array[Double], mixM: Array[Double], weight: Array[Double], swaps: Array[Double]) {
    var nbadT: Int = 0
    var nbadG: Int = 0
    var nbadM: Int = 0
  }
  object ComparedData {
    def apply(n: Int) = {
      val i = Iterator.continually(new Array[Double](max(1, n)))
      new ComparedData(i.next, i.next, i.next, i.next, i.next)
    }
  }
  
  /** Stores line fits including error estimates. */
  case class LineFit(slope: StandardDeviation, intercept: StandardDeviation, mse: StandardDeviation) {
    /** Predict y given x */
    def y(x: Double) = slope.mean*x + intercept.mean
    /** Error in y measurement (based solely on error in intercept value) */
    def semY = intercept.sem
    def n = intercept.n
    override def toString = s"Line:\n  slope     $slope\n  intercept $intercept\n  M.S.E.    $mse"
  }
  object LineFit {
    val empty = new LineFit(StdDev.empty, StdDev.empty, StdDev.empty)
    /** Compute a straight-line fit, discarding any NaN values in the input. */
    def apply(x0: Array[Double], y0: Array[Double]) = {
      val N = min(x0.length, y0.length)
      var nbad = 0
      var i = 0
      while (i < N) {
        if (!(x0(i).finite && y0(i).finite)) nbad += 1
        i += 1
      }
      val (x, y) = { if (nbad == 0) (x0, y0) else {
        val nx, ny = new Array[Double](N-nbad)
        i = 0
        var j = 0
        while (i < N) {
          if (x0(i).finite && y0(i).finite) {
            nx(j) = x0(i)
            nx(j) = y0(i)
            j += 1
          }
          i += 1
        }
        (nx, ny)
      }}
      if (x.length < 5) empty
      else {
        val fit = Nonparametric.theilsen(x,y)()
        val mse = new Array[Double](x.length)
        var i = 0
        while (i < x.length) {
          mse(i) = (y(i) - fit._1.mean*x(i) - fit._2.mean).sq
          i += 1
        }
        new LineFit(fit._1, fit._2, StdDev(mse))
      }
    }
  }
  /** Line fits for both runtime and garbage collection time. */
  case class ComparedResult(run: LineFit, gct: LineFit) { }
  object ComparedResult {
    def zero = new ComparedResult(LineFit.empty, LineFit.empty)
  }
  /** Stores information about a head-to-head benchmark between two functions. */
  class Comparison(
    /** Benchmarking data for the first function. */
    var br1: Benched,
    /** Benchmarking data for the second function. */
    var br2: Benched,
    /** Fit of various mixtures of two functions to a straight line. */
    var wtfit: ComparedResult,
    /** Fit testing whether switching from one function to another has a major impact on time taken. */
    var swapfit: ComparedResult,
    /** The actual head-to-head data */
    var data: Option[ComparedData],
    var mixSizes: (Int, Int, Int),
    /** Whether the two functions were required to be run the same number of times */
    var oneToOne: Boolean,
    /** Estimated systematic measurement error */
    var intrinsicError: Double,
    /** Total time taken making benchmarks */
    var wallClockTime: Double,
    printingPrecision0: Int = -4,
    /** Optional title for output */
    var title: String = ""
  ) {
    private[this] var myPrintingPrecision = printingPrecision0
    br1.printingPrecision = printingPrecision0
    br2.printingPrecision = printingPrecision0
    /** Reports the precision used to output times. */
    def printingPrecision = myPrintingPrecision
    /** Sets the precision used to output times. */
    def printingPrecision_=(i: Int) { myPrintingPrecision = max(-18, min(18, i)); br1.printingPrecision = myPrintingPrecision; br2.printingPrecision = myPrintingPrecision }
    /** Internal method used to detect whether alternating functions gives a different result from running one a bunch of times and then the other (results are p-values for runtime and garbage time) */
    def historyEffect: Option[(Double, Double)] = {
      def he(wt: LineFit, swap: LineFit, rmswt: Double, rmsswap: Double, n: Int): Double = {
        if (n < 5 || swap.slope.sem==0 || isNaN(swap.slope.mean) || !isFinite(swap.slope.sem)) Double.NaN
        else if (abs(cdfNormal(abs(swap.slope.mean)/swap.slope.sem)-0.5) < 0.495) Double.NaN   // 99th percentile, two-tailed
        else if (!isFinite(rmswt) || !isFinite(rmsswap)) Double.NaN
        else Parametric.regressionTest(2, rmswt, 4, rmsswap, n)
      }
      val rt = he(wtfit.run, swapfit.run, wtfit.run.mse.mean.sqrt, swapfit.run.mse.mean.sqrt, data.map(a => a.mixT.length - a.nbadT).getOrElse(0))
      val gt = he(wtfit.gct, swapfit.gct, wtfit.gct.mse.mean.sqrt, swapfit.gct.mse.mean.sqrt, data.map(a => a.mixG.length - a.nbadG).getOrElse(0))
      if (isNaN(rt) && isNaN(gt)) None else Some((rt, gt))
    }
    private[this] def fuzz(sd: StandardDeviation, error: Double = intrinsicError) = StdDev(sd.n, sd.mean, sqrt(sd.variance + error*error))
    private[this] def sem2sd(n: Int, x: Double, sem: Double, eff: Double = 1.0) = StdDev(n, x/eff, sem*sqrt(max(1,n-1))/eff)
    /** Stores a comparison of timing between two functions. */
    case class Comp(a: StandardDeviation, b: StandardDeviation, afit: StandardDeviation, bfit: StandardDeviation) {
      lazy val pdiff = a.tTest(b)
      lazy val pfitdiff = afit.tTest(bfit)
      lazy val ratio = StdDev(min(a.n, b.n), b.mean/a.mean, sqrt((a.sd*b.mean/a.mean.sq).sq + (b.sd/a.mean).sq))
      lazy val rdiff = StdDev(min(afit.n, bfit.n), bfit.mean/afit.mean, sqrt((afit.sd*bfit.mean/afit.mean.sq).sq + (bfit.sd/afit.mean).sq))
      lazy val ratdiff = rdiff.tTest(ratio)
    }
    private[this] def foldChange(brsd: Benched => StandardDeviation, w: LineFit, error: Double = intrinsicError) = {
      val e1 = mixSizes._1*mixSizes._3*br1.effort
      val e2 = mixSizes._2*mixSizes._3*br2.effort
      Comp(
        fuzz(brsd(br1), error/e1),
        fuzz(brsd(br2), error/e2),
        fuzz(sem2sd(w.n, w.y(-1), w.semY, e1), error/e1),
        fuzz(sem2sd(w.n, w.y(1), w.semY, e2), error/e2)
      )
    }
    def foldChangeRuntime = foldChange(br => sem2sd(br.runtimeIterations.toInt, br.runtime, br.runtimeError), wtfit.run, intrinsicError)
    def foldChangeGarbage = foldChange(br => sem2sd(br.garbageIterations.toInt, br.garbage, br.garbageError), wtfit.gct, max(intrinsicError, 1e-3))
    /** Estimate of overall fractional accuracy of results */
    def runtimeAccuracy = {
      val e1 = mixSizes._1 * mixSizes._3 * br1.effort
      val e2 = mixSizes._2 * mixSizes._3 * br2.effort
      val v1 = fuzz(sem2sd(wtfit.run.n, wtfit.run.y(-1), wtfit.run.semY, e1), intrinsicError/e1)
      val v2 = fuzz(sem2sd(wtfit.run.n, wtfit.run.y(1), wtfit.run.semY, e2), intrinsicError/e2)
      sqrt(v1.sem.sq + v2.sem.sq)/(0.5*(v1.mean + v2.mean))
    }
    def swapTimingImpact = {
      data.map{ cd => val s = (cd.swaps, cd.mixT).zipped.filter((a,b) => isFinite(b))._1; abs(swapfit.run.y(s.max) - swapfit.run.y(s.min)) }.getOrElse(0.0)/min(wtfit.run.y(-1),wtfit.run.y(1))
    }
    /** Multi-line description of the head-to-head benchmarking run. */
    override def toString = {
      def name1 = if (br1.title.isEmpty) "First" else br1.title
      def name2 = if (br2.title.isEmpty) "Second" else br2.title
      val maxtl = max(name1.length, name2.length)
      def ppr(p: Double) = "(p " + (if (p<1e-14) "~= 0" else (if (p<1e-3) "~= %.3e" else "~= %.4f").format(p)) + ")"
      def pwp(nm: String, sd: StandardDeviation) = {
        val prec = ceil(-log(max(sd.sem, 1e-12))).toInt+1
        s"\n    %-${maxtl}s    %s   95%% CI %s - %s".format(nm, printWithPrecision(sd.mean, pp, prec), printWithPrecision(sd.mean(0.025), pp, prec), printWithPrecision(sd.mean(0.975), pp, prec), sd.n)
      }
      def pp = printingPrecision
      val prec1 = ceil(-log(max(max(br1.intrinsicError, br2.intrinsicError),1e-9))).toInt
      val rc = foldChangeRuntime
      val gc = foldChangeGarbage
      val he = historyEffect
      "Benchmark comparison (in " + printWithPrecision(wallClockTime, pp, prec1) +")" + (if (title.isEmpty) "" else ": "+title) +
      (if (br1.title.isEmpty && br2.title.isEmpty) "" else s"\n    $name1 vs $name2") +
      "\n" + (if (rc.pfitdiff < 0.05) "Significantly " else "Not significantly ") + "different " + ppr(rc.pfitdiff) +
      "\n  Time ratio:    %.5f   95%% CI %.5f - %.5f   (n=%d)".format(rc.rdiff.mean, rc.rdiff.mean(0.025), rc.rdiff.mean(0.975), rc.rdiff.n) + pwp(name1, rc.afit) + pwp(name2, rc.bfit) +
      //"\n  Garbage ratio: %.5f   95%% CI %.5f - %.5f   (n=%d)".format(gc.rdiff.mean, gc.rdiff.mean(0.025), gc.rdiff.mean(0.975), gc.rdiff.n) + pwp(name1, gc.afit) + pwp(name2, gc.bfit) +
      (if (rc.ratdiff < 0.01) {
        "\n  Individual benchmarks not fully consistent with head-to-head " + ppr(rc.ratdiff) + pwp(name1, rc.a) + pwp(name2, rc.b)
      } else "") +
      //(if (gc.ratdiff < 0.01) {
      //  "\n  Individual time benchmarks not fully consistent with head-to-head comparison" + pwp(name1, gc.a) + pwp(name2, gc.b)
      //} else "") +
      (he match {
        case None => ""
        case Some((p1,p2)) =>
          if (p1 < 0.05) f"\nWarning: order-of-evaluation effects in timing ${ppr(p1)} of up to ${swapTimingImpact*100}%.3f%%" else ""
      })
    }
  }
  object Comparison {
    private[this] final val TripleZero = (0,0,0)
    def empty = new Comparison(Benched.empty, Benched.empty, ComparedResult.zero, ComparedResult.zero, None, TripleZero, false, 1e-3, 0.0)
  }
  
  /** Stores a resource needed for scaled benchmarking (e.g. `B` will be an array) */
  abstract class Resource[B](val N: Int) {
    /** The data */
    def data: B
    /** The number of data points to analyze */
    var n: Int = 1
    /** The index at which to start analyzing data */
    var start: Int = 0
  }
  object Resource {
    /** Given an appropriate function, allow it to compute from a resource. */
    def inline[A,B](f: (Int, Int, B) => A) = (r: Resource[B]) => f(r.start, r.n, r.data)
    /** Generate a resource on the basis of a function from `Int` */
    def apply[C](f: Int => C) = (m: Int) => { new Resource[C](m) { val data = f(m) } }  // Note: use m; n will be shadowed by n in Resource!
  }
  
  /** Stores fits for O(n^x^) and O(log(n) n^x^) given raw timing data for different sample sizes.  Experimental. */
  case class ScaleFit(val power: LineFit, val logp: LineFit, sampleSize: Int)(val xs: Array[Double], val ys: Array[Double]) {
    lazy val pLogKnown = power.mse.tTest(logp.mse)
    private var myLogPValue = 1e-3
    def minimumLogPValue = myLogPValue
    def minimumLogPValue_=(p: Double) {
      if (p > 0 && p <= 0.5) myLogPValue = p
      else throw new IllegalArgumentException("p values must be in (0, 0.5]")
    }
    var allowedFractions: Array[Double] = Array(0.5)
    private[Thyme] def fracRound(x: Double): Double = if (allowedFractions==null) x else if (x < 0 || x > 1) math.floor(x) + fracRound(x - math.floor(x)) else {
      var e = min(x, 1-x)
      var f = if (e==x) 0.0 else 1.0
      var i = 0
      while (i < allowedFractions.length) {
        val y = abs(x - allowedFractions(i))
        if (y < e) {
          e = y
          f = allowedFractions(i)
        }
        i += 1
      }
      f
    }
    private def fracRoundErr(x: Double) = abs(x - fracRound(x))
    private[Thyme] def pdiff = {
        if (logp.slope.sem==0 && power.slope.sem==0) 0.5
        else if (logp.slope.sem==0) 0.0
        else if (power.slope.sem==0) 1.0
        else {
          val p = fracRoundErr(power.slope.mean)/power.slope.sem
          val l = fracRoundErr(logp.slope.mean)/logp.slope.sem
          cdfNormal(l-p)
        }
    }
    def isLog = {
      if (pLogKnown < myLogPValue) logp.mse.mean < power.mse.mean
      else if (allowedFractions == null) (logp.mse.mean < power.mse.mean) && pLogKnown < myLogPValue
      else pdiff < myLogPValue
    }
    def used = (if (isLog) logp else power)
    def alt = (if (isLog) power else logp)
    def alpha = used.slope.mean
    def alphaError = used.slope.sem
    def constant = exp(used.intercept.mean)
    def constantError = constant*used.intercept.sem
  }
  /** Stores a report on scaling (log/linear/etc.).  Experimental. */
  class Scaled(
    var center: Int,
    var numbers: Array[Int],
    var samples: Array[Int],
    var benches: Array[Benched],
    var fits: ScaleFit,
    var wallClockTime: Double,
    printingPrecision0: Int = -4,
    var title: String = ""
  ){
    private var myPrintingPrecision = printingPrecision0 bound (-18, 18)
    def printingPrecision = myPrintingPrecision
    def printingPrecision_=(prec: Int) { myPrintingPrecision = prec bound (-18, 18) }
    override def toString = if (numbers==null || samples==null || benches==null || fits==null) "(Uninitialized Thyme.Scaled)" else {
      val prec1 = ceil(-log(max(benches.map(_.intrinsicError).max,1e-9))).toInt
      val precas = ceil(-log(max(fits.alphaError, 1e-3))).toInt
      def pp = printingPrecision
      val pwp = printWithPrecision _
      def pwpn(t: Double, pa: Int, pb: Int) = printWithPrecision(t, abs(pa), pb).split(' ').init.mkString(" ")
      def ppair(t: Double, u: Double, pa: Int, pb: Int, tab: String = "", pr: (Double,Int,Int) => String = pwp) = {
        pr(t,pa,pb) + "  " + tab + "95% CI " + pr(t+u*icdfNormal(0.025), pa, pb) + tab + ".. " + pr(t+u*icdfNormal(0.975), pa, pb)
      }
      s"Order estimation (in ${pwp(wallClockTime, pp, prec1)}):" + (if (title != null && title.length > 0) title else "") +
      s"\n  Estimate around $center (${numbers.head} to ${numbers.last})" +
      tab(
      s"\n  Best allowed fit \tO(${if (fits.isLog) "log(N) * " else ""}N^${pwpn(fits.fracRound(fits.alpha), pp, precas)})" +
      s"\n    Alpha (slope)  \t${ppair(fits.alpha, fits.alphaError, pp, precas, "\t", pwpn _)}" +
      s"\n    Constant       \t${ppair(fits.constant, fits.constantError, pp, prec1, "\t")}" 
      ) +
      s"\n  Alternate hypothesis ${pwp(exp(fits.alt.intercept.mean), pp, prec1)} * ${if (fits.isLog) "" else "Log(N) * "}N^${pwpn(fits.alt.slope.mean, pp, precas)}" +
      f"\n    p(alternate) ~= ${if (fits.allowedFractions==null) fits.pLogKnown else if (fits.isLog) fits.pdiff else 1-fits.pdiff}%.3f"
    }
  }
  object Scaled {
    def empty = new Scaled(0, null, null, null, null, 0)
  }
  
  /** A function that takes an exponential amount of time (in `n`) to compute.  Values above 30 may tax your patience. */
  def exponentialWork(n: Int, i: Int): Long = {
    if (n<=0) i.toLong
    else {
      val l = exponentialWork(n-1,i)
      val r = exponentialWork(n-1,(l&0xFFFFFFFF).toInt+1)
      (((l&0xFFFFFFFF00000000L)+(r&0xFFFFFFFF00000000L))^(r<<32L)) | ((r+1)&0xFFFFFFFF)
    }
  }
  
  /** A function that takes quadratically long in `n`. */
  def quadraticWork(n: Int): Long = {
    var s = 1L
    var i = 1
    while (i<=n) {
      var j = 1
      while (j<=n) {
        val k = i*j
        s += 1928315*k + 912837518
        j += 1
      }
      i += 1
    }
    s
  }
  
  /** A function tha takes linear time in `n` */
  def linearWork(n: Int): Long = {
    var s = 42L
    var i = 0
    while (i<n) {
      s += h2rng.nextLong
      i += 1
    }
    s
  }
  
  private[this] val h2rng = new ichi.maths.Rng.Hybrid2
  private[this] var warmedUp = false
  
  /** Warm up all the benchmarking code itself, and return an instance of Thyme.  Takes 10-30s on most machines.  Usually unnecessary, but can help reproducibility of fine distictions in timing. */
  def warmed(accuracyTarget: Double = 0.03, verbose: (String => Unit) = null) = {
    def done(dt: Double) = f"done in ${if (dt>=1) dt else dt*1e3}%.2f ${if (dt>=1) "s" else "ms"}\n"
    def verb(ta: Double, tb: Double, msg: String) {
      if (verbose != null) {
        if (tb.finite && ta.finite) verbose(done(abs(tb-ta)*1e-9))
        if (msg != null && msg.length > 0) verbose(msg)
      }
    }
    synchronized { if (warmedUp == false) {
      val t0 = System.nanoTime
      if (verbose != null) { verbose("Creating Thyme instances and warming busywork methods...") }
      val th = (for (i <- 1 to 10) yield new Thyme).sortBy(_.measurementIncrement).head
      th.targetTime = 0.05
      val lin = (for (i <- 1 to 3) yield th.Warm(linearWork(8192))).last
      val quad = (for (i <- 1 to 3) yield th.Warm(quadraticWork(256))).last
      val t1 = System.nanoTime
      verb(t0, t1, "Warming up benchmarking...")
      var bL = (for (i <- 1 to 4) yield th.benchPairWarm(lin)).sortBy(_._2.runtime).head._2
      var bQ = (for (i <- 1 to 4) yield th.benchPairWarm(quad)).sortBy(_._2.runtime).head._2
      var close = false
      var i = 0
      while (!close && i < 8) {
        val l = th.benchPairWarm(lin)._2
        val q = th.benchPairWarm(quad)._2
        if (abs(l.runtime - bL.runtime)/sqrt(l.runtimeError.sq + bL.runtimeError.sq) < icdfNormal(0.95) && abs(q.runtime - bQ.runtime)/sqrt(q.runtimeError.sq + bQ.runtimeError.sq) < icdfNormal(0.95)) close = true
        bL = l
        bQ = q
        i += 1
      }
      val t2 = System.nanoTime
      verb(t1, t2, "Warming up head-to-head benchmarking...")
      th.targetTime = 0.01
      val fastlin = th.Warm(linearWork(8192))
      val fastquad = th.Warm(quadraticWork(256))
      th.targetTime = 0.05
      var bO = (for (i <- 1 to 4) yield th.benchOffPairWarm()(fastlin)(fastquad)).sortBy(_._2.foldChangeRuntime.rdiff.mean).tail.head._2
      close = false
      i = 0
      while (!close && i < 4) {
        val (k,o) = th.benchOffPairWarm()(fastlin)(fastquad)
        if (o.foldChangeRuntime.rdiff.tTest(bO.foldChangeRuntime.rdiff) > 0.05) close = true
        bO = o
        i += 1
      }
      val t3 = System.nanoTime
      verb(t2, t3, "Warming up computational complexity benchmarking...")
      var bS = (for (i <- 1 to 3) yield th.orderPair((n: Int) => new Resource[Unit](n){ def data = () })(r => quadraticWork(r.n))(128)).sortBy(sr => abs(sr._2.fits.alpha-2)).head._2
      close = false
      i = 0
      while (!close && i < 4) {
        val s = th.orderPair((n: Int) => new Resource[Unit](n) { def data = () })(r => quadraticWork(r.n))(128)._2
        if (abs(s.fits.alpha - bS.fits.alpha)/sqrt(s.fits.alphaError.sq + bS.fits.alphaError.sq) < icdfNormal(0.95)) close = true
        bS = s
        i += 1
      }
      val t4 = System.nanoTime
      verb(t3, t4, "")
      warmedUp = true
    } }
    new Thyme(accuracyTarget)
  }
}

