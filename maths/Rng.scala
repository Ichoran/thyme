// ----------------------------------------------------------------------------
// Rng contains random number generators for Scala
// Copyright (c) 2013, Rex Kerr and the Howard Hughes Medical Institute.
// All rights reserved.
// Rng is provided as open source software under the BSD 3-clause license
// See http://www.opensource.org/licenses/bsd-license.php for text
// ----------------------------------------------------------------------------

package ichi.maths

import java.nio._
import scala.annotation.tailrec

/**
 * Rng is the common supertrait of pseudorandom number generators.  These are
 * all inherently mutable and not thread-safe.
 */
trait Rng {
  /** Sets this random number generator to its default initial state. */
  def resetToDefault: this.type
  
  /** Store the current state of the generator in an array of bytes. */
  def state: Array[Byte]
  
  /** Load stored state into the generator.  The convention is that extra bytes are ignored, and a best-effort attempt is made to use overly short arrays to perform partial initialization. */
  def seed(a: Array[Byte]): this.type
  
  /** Seed the generator with the current time.  Sequential calls from the same thread produce different seeds. */
  def seedWithTime: this.type = {
    var lastt = 0L
    var t = 0L
    while (t==lastt) { t = System.nanoTime }
    val r = (new Rng.Marsaglia64x3).seedL(t)
    val buf = state
    val bb = ByteBuffer.wrap(state).order(Rng.endian)
    while (bb.remaining >= 8) bb.putLong(r.nextLong)
    while (bb.remaining >= 4) bb.putInt(r.nextInt)
    var i = r.nextInt
    while (bb.hasRemaining) { bb.put((i&0xFF).toByte); i >>> 8 }
    seed(buf)
  }
  
  /** Return the next Int from this random number stream. */
  def nextInt: Int
  
  /** Return the next Long from this random number stream. */
  def nextLong: Long
  
  /** Return the next Double in the range [0,1) from this random number stream.  Values are equally spaced.  Based on nextLong shifted down to 53 bits. */
  def nextDouble: Double = (nextLong >>> 11)*1.1102230246251565e-16
  
  /** Return the next Float in the range [0,1) from this random number stream.  Values are equally spaced.  Based on nextInt shifted down to 24 bits. */
  def nextFloat: Float = (nextInt >>> 8)*5.9604645e-8f
  
  /** Produce a number in {0, 1, ..., n-1} */
  def roll(n: Int) = math.floor(n*nextDouble).toInt
  
  /** 
   * Produce a Normally distributed number with mean 0, standard deviation 1. 
   * Uses Leva's ratio-of-uniforms method as presented in Thomas et al. ACM Computing Surveys v39 n4 article 11 (2007);
   * produces high-quality Normal numbers fairly quickly without requiring stored state.
   * Order of operations optimized for Xeon X5680.  This cannot be overridden so that it can be tail-recursive.
   */
  @tailrec final def nextNormal: Double = {
    val v = 1.71552776992141359*nextDouble - 0.857763884960706796
    val y = math.abs(v) + 0.386595
    val u = nextDouble
    val x = u - 0.449871
    val q = x*x + (0.196*y - 0.25472*x)*y
    if (q < 0.27597) v/u
    else if (q < 0.27846 && v*v < -4*u*u*math.log(u)) v/u
    else nextNormal
  }
  
  /** Produce a normally distributed number with the supplied mean and standard deviation */
  final def nextNormal(mean: Double, sigma: Double): Double = nextNormal*sigma + mean
  
  /** Produce an endless iterator of Ints */
  def ints = Iterator.continually(nextInt)
  
  /** Produce an endless iterator of Longs */
  def longs = Iterator.continually(nextLong)
  
  /** Produce an endless iterator of Doubles */
  def doubles = Iterator.continually(nextDouble)
  
  /** Produce an endless iterator of Floats */
  def floats = Iterator.continually(nextFloat)
  
  /** Produce an endless iterator of numbers each drawn from {0, 1, ..., n-1} */
  def rolls(n: Int) = Iterator.continually(roll(n))
  
  /** Produce an endless iterator of Normally distributed numbers with mean 0 and deviation 1 */
  def normals = Iterator.continually(nextNormal)
  
  /** Produce an endless iterator of Normally distributed numbers with a particular mean and deviation */
  def normals(mean: Double, sigma: Double) = Iterator.continually(nextNormal(mean, sigma))
}


/**
 * A trait for random number generators storing a Long's worth of state.  Assume that the generator produces 64 bits per cycle.
 */
trait RngLongState extends Rng {
  /** The current state packed in a Long */
  def stateL: Long
  
  /** Set the current state to `l` */
  def seedL(l: Long): this.type
  
  def state = Rng.longBytes(stateL)
  
  def seed(a: Array[Byte]) = {
    val bb = ByteBuffer.wrap(a).order(Rng.endian)
    seedL(if (bb.remaining >= 8) bb.getLong else Rng.drainBufferToLong(bb))
  }
  
  /** Return the next Int from this random number stream by producing a Long and discarding the lower 32 bits. */
  def nextInt = (nextLong>>>32).toInt
}

/**
 * A trait for random number generators storing an Int's worth of state.  Assume that the generator produces 32 bits per cycle.
 */
trait RngIntState extends Rng {
  /** The current state packed in an Int */
  def stateI: Int
  
  /** Set the current state to `i` */
  def seedI(i: Int): this.type
  
  def state = Rng.intBytes(stateI)
  
  def seed(a: Array[Byte]) = { 
    val bb = ByteBuffer.wrap(a).order(Rng.endian)
    seedI(if (bb.remaining >= 4) bb.getInt else Rng.drainBufferToInt(bb))
  }
  
  /** Return the next Long from this random number stream by combining two Ints.  The first one is shifted high. */
  def nextLong = (nextInt.toLong << 32) | nextInt
}

/**
 * A trait for random number generators storing two Longs' worth of state.
 */
trait RngLong2State extends Rng {
  /** The first half of the current state packed into a Long */
  def stateL1: Long
  
  /** The second half of the current state packed into a Long */
  def stateL2: Long
  
  /** Sets the state of the generator with two Longs. */
  def seedLL(l1: Long, l2: Long): this.type
  
  def state = Rng.longBytes(stateL1, stateL2)
  
  def seed(a: Array[Byte]) = {
    val bb = ByteBuffer.wrap(a).order(Rng.endian)
    seedLL(
      if (bb.remaining >= 8) bb.getLong else Rng.drainBufferToLong(bb),
      if (bb.remaining >= 8) bb.getLong else Rng.drainBufferToLong(bb)
    )
  }
}

/**
 * A trait for random number generators storing three Longs' worth of state.
 */
trait RngLong3State extends Rng {
  /** The first third of the current state packed into a Long */
  def stateL1: Long
  
  /** The second third of the current state packed into a Long */
  def stateL2: Long
  
  /** The final third of the current state packed into a Long */
  def stateL3: Long
  
  /** Sets the state of the generator with three Longs. */
  def seedLLL(l1: Long, l2: Long, l3: Long): this.type
  
  def state = Rng.longBytes(stateL1, stateL2, stateL3)
  
  def seed(a: Array[Byte]) = {
    val bb = ByteBuffer.wrap(a).order(Rng.endian)
    seedLLL(
      if (bb.remaining >= 8) bb.getLong else Rng.drainBufferToLong(bb),
      if (bb.remaining >= 8) bb.getLong else Rng.drainBufferToLong(bb),
      if (bb.remaining >= 8) bb.getLong else Rng.drainBufferToLong(bb)
    )
  }
}


/**
 * Utility methods and random number implementations.
 */
object Rng {
  /** The byte order used to pack internal state into byte arrays. (Set to java.nio.ByteOrder.LITTLE_ENDIAN.) */
  val endian = ByteOrder.LITTLE_ENDIAN
  /** Pack some number of Ints into a byte array. */
  def intBytes(i: Int*) = {
    val bb = ByteBuffer.allocate(4*i.length).order(endian)
    i.foreach(bb.putInt)
    bb.array
  }
  /** Pack some number of Longs into a byte array. */
  def longBytes(l: Long*) = {
    val bb = ByteBuffer.allocate(8*l.length).order(endian)
    l.foreach(bb.putLong)
    bb.array
  }
  /** Given an array of bytes, produce an IntBuffer that delivers it as Ints (any remainder is lost). */
  def bytesInt(a: Array[Byte]) = {
    val b = if (a.length<4) java.util.Arrays.copyOf(a,4) else a
    ByteBuffer.wrap(b).order(endian).getInt
  }
  /** Given an array of bytes, produce a LongBuffer that delivers it as Longs (any remainder is lost). */
  def bytesLong(a: Array[Byte]) = {
    val b = if (a.length<8) java.util.Arrays.copyOf(a,8) else a
    ByteBuffer.wrap(b).order(endian).getLong
  }
  
  private[maths] def drainBufferToLong(b: ByteBuffer) = { var j = 0L; while (b.hasRemaining) { j = (j<<8) | (b.get) }; j }
  private[maths] def drainBufferToInt(b: ByteBuffer) = { var j = 0; while (b.hasRemaining) { j = (j<<8) | (b.get) }; j }
  
  /** Linear congruential generator with 2^32 modulus.  Extremely fast but poor quality of randomness. */
  class Lcg32() extends RngIntState {
    private[this] var seed: Int = 0
    def resetToDefault = { seed = 0; this }
    def nextInt = { seed = 1664525*seed + 1013904223; seed }    // Numbers as in Numerical Recipes in C, 2nd ed.
    def stateI = seed
    def seedI(i: Int) = { seed = i; this }
  }
  object Lcg32 {
    /** Create a new Lcg32 generator. */
    def apply() = new Lcg32()
    private[this] val rng = new Lcg32()
    /** Apply the Lcg32 algorithm to an Int of your choice. */
    def step(i: Int) = 1664525*i + 1013904223
    /** Deliver the next Int in the default stream with the default seed.  Thread-safe.  Primarily for convenience; not randomized on startup and synchronization slows things down. */
    def nx = synchronized { rng.nextInt }
  }
  
  /** Linear congruential generator with 2^32 modulus.  Extremely fast, moderately random.  Based on Mascagni SPRNG lcg64 values. */
  class Lcg64() extends RngLongState {
    private[this] var seed: Long = 0
    def resetToDefault = { seed = 0; this }
    def nextLong = { seed = seed*2862933555777941757L + 3037000493L; seed }
    def stateL = seed
    def seedL(l: Long) = { seed = l; this }
  }
  object Lcg64 {
    /** Create a new Lcg64 generator. */
    def apply() = new Lcg64()
    private[this] val rng = new Lcg64()
    /** Apply the Lcg64 algorithm to a Long of your choice. */
    def step(l: Long) = l*2862933555777941757L + 3037000493L
    /** Deliver the next Long in the default stream with the default seed.  Thread-safe.  Primarily for convenience; not randomized on startup and synchronization slows things down. */    
    def nx = synchronized { rng.nextLong }
  }
  
  /**
   * Linear congruential generator with period 2^62^ - 2^16^.
   * Moderately fast, decently random, but does not generate a full 64 bits of randomness per iteration.
   * The seed value 0L is a fixed point, so a default value is used instead.
   * Values from [[http://nuclear.llnl.gov/CNP/rng/rngman/node9.html]]
   */
  class Lcg62() extends RngLongState {
    private[this] var myState = (62L << 32)
    def resetToDefault = { myState = (62L << 32); this }
    private[this] def myNext = { myState = (3355703948966806692L*myState) % 4611686018427322369L; if (myState<0) myState + 4611686018427322368L else myState - 1  }
    /** Return the next number from this random number stream, in {0, 1, ..., 2^62^-2^16^-1}. */
    def nextUnderlying = myNext
    /** Return the next Int from this random number stream by taking the intrinsically generated number and dividing.  Produces ~0.0061% too few -1's. */
    override def nextInt = (myNext/1073741824L).toInt
    /** Return the next Long from this random number stream by taking the intriniscally generated number and scrambling it with the Mascagni SPRNG lcg64. */
    def nextLong = myNext*2862933555777941757L + 3037000493L
    /** Return the next Double from this random number stream by scaling intrinsically generated number. */
    override def nextDouble = myNext * 2.1684043449710396824e-19
    /** Return the next Float from this random number stream by scaling intrinsically generated number. */
    override def nextFloat = (myNext * 2.1684043449710396824e-19).toFloat
    def stateL = myState
    def seedL(l: Long) = { myState = l % 4611686018427322369L; if (myState<0) { myState += 4611686018427322369L }; if (myState==0) { myState = 1 }; this }
  }
  object Lcg62 {
    /** Create a new Lcg62 generator. */
    def apply() = new Lcg62()
    private[this] val rng = new Lcg62()
    /** Apply the Lcg62 algorithm to a Long of your choice. Note: 1 is not subtracted, so 0 will not be reached unless the initial value is congruent to 0 mod 2^62^-2^16^+1 in which case it will always be 0. */
    def step(l: Long) = { val j = (3355703948966806692L*l) % 4611686018427322369L; if (j<0) { j + 4611686018427322369L } else { j } }
    /** Deliver the next number in the default stream with the default seed.  Thread-safe.  Primarily for convenience; not randomized on startup and synchronization slows things down. */
    def nx = synchronized { rng.nextUnderlying }
  }

  /**
   * George Marsaglia XorShift RNG with 32 bits of state and 3 shifts.  From [[http://www.jstatsoft.org/v08/i14/paper]].
   * Very fast and reasonably random for 32 bits of state, but intrinsically cannot generate 0.
   */
  class Marsaglia32x3() extends RngIntState {    // -1831433054 == unsigned 2463534242
    private[this] var myState: Int = -1831433054
    def resetToDefault = { myState = -1831433054; this }
    /** Return the next Int in the stream.  0 cannot be generated, so 1 is subtracted to recover it.  (Thus, -1 cannot be generated.) */
    def nextInt = { myState ^= myState<<13; myState ^= myState>>17; myState ^= myState<<5; myState-1 }
    /** Return the next Long in the stream.  Better to use Marsaglia64x3 if you need Longs--this requires three Ints to avoid weird glitches due to lack-of-zero. */
    override def nextLong = { ((nextInt >>> 16) << 56L) | ((nextInt>>>8) << 24L) | (nextInt >> 8) }
    /** Return the next Double in the stream.  Uses two ints to generate 53 random bits. Symmetric, but due to missing zero a small number of values are ~3% underrepresented. */
    override def nextDouble = (((nextInt >>> 6)<<27L)|(nextInt>>>5))*1.1102230246251565e-16
    def stateI = myState
    /** Set the state to ''i''.  Zero is a degenerate value, so it is promoted to 1. */
    def seedI(i: Int) = { if (i == 0) { resetToDefault } else { myState = i }; this }
  }
  object Marsaglia32x3 {
    /** Create a new Marsaglia32x3 generator. */
    def apply() = new Marsaglia32x3()
    private[this] val rng = new Marsaglia32x3
    /** Apply the Marsaglia 32 bit XorShift with values <<13 >>17 <<5 to an Int of your choice.  0 will stay 0.  Result does not have 1 subtracted (so is in the range 1 to 0xFFFFFFFF). */
    def step(i: Int) = { var seed = i; seed ^= seed<<13; seed ^= seed>>17; seed ^= seed<<5; seed }
    /** Deliver the next Int in the default stream with the default seed.  Thread-safe.  Primarily for convenience; not randomized on startup and synchronization slows things down. */    
    def nx = synchronized{ rng.nextInt }
  }
  
  /**
   * George Marsaglia XorShift RNG with 64 bits of state and 3 shifts.  From [[http://www.jstatsoft.org/v08/i14/paper]].
   * Very fast and quite random, but intrinsically cannot generate 0.
   */
  class Marsaglia64x3()  extends RngLongState {
    private[this] var myState: Long = 88172645463325252L
    def resetToDefault = { myState = 88172645463325252L ; this }
    /** Return the next Long in the stream.  0 cannot be generated as it is a fixed point. */
    def nextLong = { myState ^= myState<<21; myState ^= myState>>>35; myState ^= myState<<4; myState }   // Using Numerical Recipes' favorite values instead of Marsaglia's (from among Marsaglia's list)
    def stateL = myState
    def seedL(l: Long) = { if (l != 0) { myState = l } else { resetToDefault } ; this }
  }
  object Marsaglia64x3 {
    /** Create a new Marsaglia64x3 generator with a Long seed.  Zero is degenerate so is promoted to 1. */
    def apply() = new Marsaglia64x3()
    private[this] val rng = new Marsaglia64x3
    /** Apply the Marsaglia 64 bit XorShift with values <<21 >>35 <<4 to an Int of your choice.  0 will stay 0.  Result does not have 1 subtracted (so is in the range 1 to 0xFFFFFFFFFFFFFFFFL). */
    def step(i: Int) = { var seed = i; seed ^= seed<<21; seed ^= seed>>>35; seed ^= seed<<4; seed }
    /** Deliver the next Long in the default stream with the default seed.  Thread-safe.  Primarily for convenience; not randomized on startup and synchronization slows things down. */
    def nx = rng.nextLong
  }
  
  /**
   * Marsaglia "Weyl sequence" RNG with cycle length of 2^192^ - 2^32^ from [[http://www.jstatsoft.org/v08/i14/paper]].
   * Quite fast and quite random; requires 24 bytes of state.
   */
  class Marsaglia32a6() extends RngLong3State {
    private[this] var x: Int = 123456789
    private[this] var y: Int = 362436069
    private[this] var z: Int = 521288629
    private[this] var w: Int = 88675123
    private[this] var v: Int = 5783321
    private[this] var d: Int = 6615241
    def resetToDefault = {
      x = 123456789; y = 362436069; z = 521288629; w = 88675123; v = 5783321; d = 6615241
      this
    }
    def stateL1 = (x.toLong<<32) | y
    def stateL2 = (z.toLong<<32) | w
    def stateL3 = (v.toLong<<32) | d
    def seedLLL(l1: Long, l2: Long, l3: Long) = {
      x = (l1>>>32).toInt
      y = (l1&0xFFFFFFFFL).toInt;
      z = (l2>>32).toInt
      w = (l2&0xFFFFFFFFL).toInt        
      v = (l3>>32).toInt
      d = (l3&0xFFFFFFFFL).toInt
      if (x==0 && y==0 && z==0 && w==0 && v==0) {
        val d0 = d
        resetToDefault
        d = d0
      }
      this
    }
    def nextInt = {
      val t = x ^ (x>>>2)
      x = y
      y = z
      z = w
      w = v
      v = v ^ (v<<4) ^ (t ^ (t<<1))
      d += 362437
      d + v
    }
    def nextLong = (nextInt.toLong << 32) | nextInt
  }
  object Marsaglia32a6 {
    /** Create a new Marsaglia32a6 generator. */
    def apply() = new Marsaglia32a6
    private[this] val rng = new Marsaglia32a6
    /** Deliver the next Int in the default stream with the default seeds.  Thread-safe.  Primarily for convenience; not randomized on startup and synchronization slows things down. */
    def nx = synchronized { rng.nextInt }
  }
    

  /**
   * Bit-mixing random number generator based on rotations from Bob Burtle.  Maintains 16 bytes of state information.
   * Algorithm from [[http://burtleburtle.net/bob/rand/]]
   */
  abstract class BurtleRot32() extends RngLong2State {
    protected var a = 0x131fa903
    protected var b = 0x927aed26
    protected var c = 0xa696f285
    protected var d = 0x750a9db8
    def resetToDefault = {
      a = 0x131fa903; b = 0x927aed26; c = 0xa696f285; d = 0x750a9db8
      this
    }
    def stateL1 = (a.toLong<<32) | b
    def stateL2 = (c.toLong<<32) | d
    def seedLL(l1: Long, l2: Long): this.type = {
      if (l1==0 && l2==0) resetToDefault
      else {
        a = (l1>>>32).toInt
        b = (l1&0xFFFFFFFFL).toInt;
        c = (l2>>32).toInt
        d = (l2&0xFFFFFFFFL).toInt
      }
      this
    }
    def nextInt = { advance(); d }
    def nextLong = (nextInt.toLong << 32) | nextInt
    /** Performs rotation/mixing/storage. */
    protected def advance(): Unit
  }
  
  /**
   * Bit-mixing random number generator based on rotations from Bob Burtle.  Maintains 16 bytes of state information.  Good speed and randomness (see `Burtle3rot` for better randomness).
   * Algorithm from [[http://burtleburtle.net/bob/rand/]]
   */
  final class Burtle2rot() extends BurtleRot32 {
    protected def advance() {
      val e = a - java.lang.Integer.rotateLeft(b,27)
      a = b ^ java.lang.Integer.rotateLeft(c,17)
      b = c + d
      c = d + e
      d = e + a
    }
  }
  object Burtle2rot {
    /** Create a new Burtle2rot generator. */
    def apply() = (new Burtle2rot)
    private[this] val rng = new Burtle2rot
    /** Deliver the next Int in the default stream with the default seeds.  Thread-safe.  Primarily for convenience; not randomized on startup and synchronization slows things down. */
    def nx = synchronized { rng.nextInt }
  }
  
  /**
   * Bit-mixing random number generator based on rotations from Bob Burtle.  Maintains 16 bytes of state information.  Decent speed and very good randomness (see `Burtle2rot` for better speed).
   * Algorithm from [[http://burtleburtle.net/bob/rand/]]
   */
  final class Burtle3rot() extends BurtleRot32 {
    protected def advance() {
      val e = a - java.lang.Integer.rotateLeft(b,23)
      a = b ^ java.lang.Integer.rotateLeft(c,16)
      b = c + java.lang.Integer.rotateLeft(d,11)
      c = d + e
      d = e + a      
    }
  }
  object Burtle3rot {
    /** Create a new Burtle3rot generator.  All zeros is degenerate so are promoted to default values. */
    def apply() = (new Burtle2rot)
    private[this] val rng = new Burtle3rot
    /** Deliver the next Int in the default stream with the default seeds.  Thread-safe.  Primarily for convenience; not randomized on startup and synchronization slows things down. */
    def nx = synchronized { rng.nextInt }
  }
  
  
  /**
   * Hybrid (combined) RNG suggested by Numerical Recipes in C, 3rd. ed. (where it is called Ranq1); combines Lcg64 with Marsaglia64x3.  When in doubt, use this!  Cannot return the value
   * 0 due to a period of only 2^64^-1, but this shouldn't matter.
   */
  class Hybrid2() extends RngLongState {
    private[this] var myState: Long = 4101842887655102017L
    def resetToDefault = { myState = 4101842887655102017L; this }
    def stateL = myState
    def seedL(l: Long): this.type = { if (l==0) { resetToDefault } else { myState = l }; this }
    /** Return the next Long in the stream.  0 cannot be generated as it is a fixed point. */
    def nextLong = { myState ^= myState >>> 21; myState ^= myState << 35; myState ^= myState >>>4; myState * 2685821657736338717L }
  }
  object Hybrid2 {
    /** Create a new Hybrid2 generator. */
    def apply() = new Hybrid2()
    private[this] val rng = new Hybrid2
    /** Apply the Hybrid2 algorithm to a Long of your choice. */
    def step(l: Long) = { var seed = l; seed ^= seed >>> 21; seed ^= seed << 35; seed ^= seed >>>4; seed * 2685821657736338717L }
    /** Deliver the next Long in the default stream with the default seeds.  Thread-safe.  Primarily for convenience; not randomized on startup and synchronization slows things down. */
    def nx = synchronized{ rng.nextLong }
  }
  
  /**
   * High-quality hybrid (combined) RNG suggested by Numerical Recipes in C, 3rd. ed. (where it is called Ran);
   * combines two variants of Marsaglia64x3 with one Lcg64 and one multiply-and-carry.
   * Requires 24 bytes of state.  Only slightly slower than `Hybrid2` on the JVM on a Xeon 5680.
   */
  class Hybrid4() extends RngLong3State {
    private[this] var ma = 4101842887655102017L
    private[this] var lc = 102030405060708090L
    private[this] var xc = 1L
    def resetToDefault = {
      ma = 4101842887655102017L; lc = 102030405060708090L; xc = 1L
      this
    }
    def stateL1 = ma
    def stateL2 = lc
    def stateL3 = xc
    def seedLLL(l1: Long, l2: Long, l3: Long) = {
      ma = if (l1==0) 4101842887655102017L else l1
      lc = l2
      xc = if (l3==0) 1L else l3
      this
    }
    def nextLong = {
      ma ^= ma >>> 17; ma ^= ma << 31; ma ^= ma >>> 8
      lc = lc * 286293555777941757L + 7046929254386353087L
      xc = 4294957665L * (xc & 0xFFFFFFFFL) + (xc >>> 32)
      var mb = lc ^ (lc << 21); mb ^= mb >>> 35; mb ^= mb << 4
      (mb + ma) ^ xc
    }
    def nextInt = (nextLong>>32).toInt
  }
  object Hybrid4 {
    /** Create a new Hybrid4 generator. */
    def apply() = new Hybrid4()
    private[this] val rng = new Hybrid4()
    
    /** Deliver the next Long in the default stream with the default seeds.  Thread-safe.  Primarily for convenience; not randomized on startup and synchronization slows things down. */
    def nx = synchronized{ rng.nextLong }
  }
  
  def permuteInPlace[@specialized T](a: Array[T], r: Rng) = {
    var i = 0
    while (i+1 < a.length) {
      val j = r.roll(a.length - i)
      if (j > 0) {
        val temp = a(i+j)
        a(j+i) = a(i)
        a(i) = temp
      }
      i += 1
    }
    a
  }
  def permuteInPlace[@specialized T](a: Array[T], r: Rng, i0: Int, i1: Int, step: Int) = {
    var n = (step+i1-i0)/step
    var i = i0
    while (n > 1) {
      val j = i + r.roll(n)*step
      if (j>i) {
        val temp = a(j)
        a(j) = a(i)
        a(i) = temp
      }
      i += step
      n -= 1
    }
    a
  }
  def permute[@specialized T](a: Array[T], r: Rng) = permuteInPlace(a.clone, r)
  
  def subsampleInto[@specialized T](a: Array[T], r: Rng, b: Array[T], start: Int, n: Int) = {
    var i,j = 0
    if (n < a.length) {
      while (j<n) {
        if (r.nextDouble*(a.length-i) <= (n-j)) {
          b(j) = a(i)
          j += 1
        }
        i += 1
      }
    }
    else {
      while (j < n) {
        b(j) = a(i)
        j += 1
        if (r.nextDouble*(n-j) < (a.length-i)) i += 1
      }
    }
    b
  }
  def subsampleInto[@specialized T](a: Array[T], r: Rng, b: Array[T]): Array[T] = subsampleInto(a,r,b,0,b.length)   
}
