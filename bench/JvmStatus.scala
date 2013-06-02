// ----------------------------------------------------------------------------
// JvmStatus reads and stores JVM status information for Thyme
// Copyright (c) 2013, Rex Kerr.  All rights reserved.
// This code is provided as open source software under the BSD 3-clause license
// See http://www.opensource.org/licenses/bsd-license.php for text
// ----------------------------------------------------------------------------


package ichi.bench

import java.lang.management._

/** JvmStatus abstracts measurements of JVM status relevant to timing and benchmarking.
 * The status may be absolute or a difference of measurements.
 */
sealed trait JvmStatus extends Cloneable {
  /** True if the status is a difference between two measurements.  False otherwise. */
  def isDiff: Boolean
  
  /** The value of `System.nanoTime` at the beginning of a status measurement, if `isDiff` is false.  If `true`, it is the time from the end
   * of the first status measurement to the beginning of the second status measurement, i.e. the time taken not counting status measurements (aside
   * from the `System.nanoTime` calls themselves).
   */
  def nsTicks: Long
  
  /** The value of `System.nanoTime` at the end of a status measurement, or the time elapsed counting status measurements. */
  def nsTocks: Long
  
  /** Number of classes loaded */
  def clLoads: Long
  
  /** Amount of time spent in garbage collection, in milliseconds */
  def gcTime: Long
  
  /** Number of invocations of the garbage collector (sum across all types of collection) */
  def gcCount: Long
  
  /** Amount of memory in use--note that this is highly unreliable on most JVMs */
  def memUse: Long
  
  /** If true, time was/will be checked. */
  def checkT: Boolean
  
  /** If true, garbage collection was/will be checked. */
  def checkGC: Boolean
  
  /** If true, class loads was/will be checked. */
  def checkCL: Boolean
  
  /** If true, memory allocation will be checked. */
  def checkMem: Boolean
  
  /** Stub to make Cloneable happy--Imm and Mut classes contain actual implementations */
  override def clone(): JvmStatus = ???
  
  /** An immutable version of this status. */
  def immutable: JvmStatusImm
  
  /** A mutable version of this status (will not make a copy unless the current version is immutable) */
  def mutable: JvmStatusMut
  
  /** Update the current object or generate a new one (depending on mutability) that captures the current status. */
  def current(): JvmStatus
  
  /** Compute the difference in status between two measurements (expected usage is later `diff` earlier).
   *If immutable, a new object will be created.  If mutable, the calling object will update and return itself. */
  def diff(js: JvmStatus): JvmStatus
}

/** An immutable representation of JVM status. Used for stability. */
final case class JvmStatusImm(isDiff: Boolean, nsTicks: Long, nsTocks: Long, clLoads: Long, gcTime: Long, gcCount:Long, memUse: Long)(
  val checkT: Boolean, val checkGC: Boolean, val checkCL: Boolean, val checkMem: Boolean
) extends JvmStatus {
  def immutable: this.type = this
  def mutable = (new JvmStatusMut) := this
  override def clone(): JvmStatusImm = this
  def current() = mutable.current.immutable
  def diff(js: JvmStatus) = mutable.diff(js).immutable
  
  /** Adds two differential status measurements. */
  def +(js: JvmStatus) = (mutable += js).immutable
  
  /** Subtracts two differential status measurements.  Do not use this in place of `diff` to generate a differential measurement from absolute measurements! */
  def -(js: JvmStatus) = (mutable -= js).immutable
  
  /** Multiply the status measurements by a constant. */
  def *(d: Double) = (mutable *= d).immutable
}

/** A mutable representation of JVM status. Used for efficiency. */
final class JvmStatusMut() extends JvmStatus {
  import JvmStatus._
  var isDiff = false
  var nsTicks = 0L
  var nsTocks = 0L
  var clLoads = 0L
  var gcTime = 0L
  var gcCount = 0L
  var memUse = 0L
  var checkT = true
  var checkGC = true
  var checkCL = true
  var checkMem = false
  def enableT(b: Boolean): this.type = { checkT = b; this }
  def enableGC(b: Boolean): this.type = { checkGC = b; this }
  def enableCL(b: Boolean): this.type = { checkCL = b; this }
  def enableMem(b: Boolean): this.type = { checkMem = b; this }
  def immutable = new JvmStatusImm(isDiff, nsTicks, nsTocks, clLoads, gcTime, gcCount, memUse)(checkT, checkGC, checkCL, checkMem)
  def mutable = this
  override def clone(): JvmStatusMut = (new JvmStatusMut) := this
  def current(): this.type = {
    nsTicks = if (checkT) System.nanoTime else 0L
    isDiff = false
    clLoads = (if (checkCL) cl.getTotalLoadedClassCount else 0)
    gcTime = 0L
    gcCount = 0L
    if (checkGC) {
      var i = 0
      while (i < gcs.length) {
        gcTime += gcs(i).getCollectionTime
        gcCount += gcs(i).getCollectionCount
        i += 1
      }
    }
    memUse = (if (checkMem) mem.getHeapMemoryUsage.getUsed else 0)
    nsTocks = if (checkT) System.nanoTime else 0L
    this
  }
  
  /** Set the current status to the values of another status object.  Returns the current (modified) status object. */
  def :=(js: JvmStatus): this.type = {
    isDiff = js.isDiff
    clLoads = js.clLoads
    gcTime = js.gcTime
    gcCount = js.gcCount
    memUse = js.memUse
    checkGC = js.checkGC
    checkCL = js.checkCL
    checkMem = js.checkMem
    this
  }
  
  /** Add another differential status to this one.  Returns the current (modified) status object. */
  def +=(js: JvmStatus): this.type = {
    if (isDiff != js.isDiff) throw new IllegalArgumentException("Can only perform arithmetic on statuses of the same type")
    nsTicks += js.nsTicks
    nsTocks += js.nsTocks
    clLoads += js.clLoads
    gcTime += js.gcTime
    gcCount += js.gcCount
    memUse += js.memUse
    this
  }
  
  /** Subtract another differential status from this one.  Returns the current (modified) status object. */
  def -=(js: JvmStatus): this.type = {
    if (isDiff != js.isDiff) throw new IllegalArgumentException("Can only perform arithmetic on statuses of the same type")
    nsTicks -= js.nsTicks
    nsTocks -= js.nsTocks
    clLoads -= js.clLoads
    gcTime -= js.gcTime
    gcCount -= js.gcCount
    memUse -= js.memUse
    this
  }
  
  /** Multiply this status by a constant.  Returns the current (modified) status object. */
  def *=(d: Double): this.type = {
    if (java.lang.Double.isNaN(d) || d.isInfinity) throw new IllegalArgumentException("Must perform finite scaling on status counts")
    nsTicks = math.round(nsTicks*d)
    nsTocks = math.round(nsTocks*d)
    clLoads = math.round(clLoads*d)
    gcTime = math.round(gcTime*d)
    gcCount = math.round(gcCount*d)
    memUse = math.round(memUse*d)
    this
  }
  
  def diff(js: JvmStatus): this.type = {
    if (isDiff || js.isDiff) throw new IllegalArgumentException("Must take a difference of absolute status measurements")
    nsTicks -= js.nsTocks
    nsTocks -= js.nsTicks
    clLoads -= js.clLoads
    gcTime -= js.gcTime
    gcCount -= js.gcCount
    memUse -= js.memUse
    checkT &&= js.checkT
    checkGC &&= js.checkGC
    checkCL &&= js.checkCL
    checkMem &&= js.checkMem
    isDiff = true
    this
  }
}

/** Holds factory objects to collect status information. */
object JvmStatus {
  /** Garbage collection monitors */
  val gcs = {
    val xs = ManagementFactory.getGarbageCollectorMXBeans
    xs.toArray(new Array[GarbageCollectorMXBean](xs.size))
  }
  
  /** Class loading monitor */
  val cl = ManagementFactory.getClassLoadingMXBean
  
  /** Memory subsystem monitor */
  val mem = ManagementFactory.getMemoryMXBean
  
  /** An empty immutable status object */
  val immutable = new JvmStatusImm(false, 0, 0, 0, 0, 0, 0)(true, true, true, false)
  
  /** A new mutable status object without any status information.  Call `current` on it to populate it with status information. */
  def mutable = new JvmStatusMut()
  
  /** The current status in an immutable object */
  def status = (new JvmStatusMut()).current.immutable
}

