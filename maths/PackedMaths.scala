// ----------------------------------------------------------------------------
// PackedMaths contains value classes that pack small primitives in larger ones
// Copyright (c) 2013, Rex Kerr and the Howard Hughes Medical Institute.
// All rights reserved.
// PackedMaths is open source software under the BSD 3-clause license
// See http://www.opensource.org/licenses/bsd-license.php for text
// ----------------------------------------------------------------------------

package ichi.maths

import scala.annotation.tailrec

object PackedMaths {
  def packHalf(f: Float): Short = {
    val b = java.lang.Float.floatToIntBits(f)
    val s = (b>>>16)&0x8000
    val v = b&0x7FFFFFFF
    if (v >= 0x477FF000) {                                      // 0x477FF000 = 47800000 which is critical value, minus 0x1000 == bit 12 of 23 == -1 of 10 == "+0.5" for rounding
      if (v > 0x7F800000) (s | (v>>13) | 0x7C01).toShort        // Keep signaling bit of NaN if present, ensure it is a NaN by adding a 1
      else (s | 0x7C00).toShort                                 // Simply too big, just Inf it
    }
    else if (v >= 0x387FF000) (s | (v-0x37FFF000)>>13).toShort  // Normalized form critical value (including rounding), with appropriate fix
    else if (v < 0x32FFF000) s.toShort
    else {
      val e = (b>>>23)&0xFF
      (s | (((0x800000 | (b&0x7FFFFF)) + (0x800000 >>> (e-102))) >>> (126-e))).toShort
    }
  }
  def unpackHalf(s: Short): Float = {
    val exp = (s&0x7C00)
    if (exp==0x7C00) java.lang.Float.intBitsToFloat( ((s&0x8000)<<16) | 0x7F800000 | (s&0x03FF) )            // 0x7F800000 is filled float exponent
    else if (exp!=0) java.lang.Float.intBitsToFloat( ((s&0x8000)<<16) | (((s&0x7FFF)<<13) + 0x38000000) )    // 0x38000000 is exponent sign correction factor
    else {
      val mant = (s&0x03FF)
      if (mant==0) java.lang.Float.intBitsToFloat( (s&0x8000)<<16 )
      else 5.9604645e-8f * (if ((s&0x8000)==0) mant else -mant)       // Subnormals are 2^-24 times mantissa
    }
  }
  def isHalfNaN(s: Short) = (s&0x7C00)==0x7C00 && (s&0x3FF)!=0
  val halfNaN = 0x7C01: Short

  final class BytePack(val repr: Byte) extends AnyVal {
    def n0 = ((repr << 4).toByte >> 4).toByte
    def n0(n: Byte) = new BytePack(((repr&0xF0) | (n&0xF)).toByte)
    def n1 = (repr >> 4).toByte
    def n1(n: Byte) = new BytePack((((n<<4)&0xF0) | (repr&0xF)).toByte)
    def swapN = new BytePack((((repr&0xF0)>>4) | ((repr&0xF)<<4)).toByte)
  }
  implicit final class ByteAsPacked(val b: Byte) extends AnyVal {
    def packB = new BytePack(b)
    def packN(c: Byte) = new BytePack(((b&0xF) | (c<<4)).toByte)
  }
  
  final class ShortPack(val repr: Short) extends AnyVal {
    @inline private def s = repr
    def b0 = (s & 0xFF).toByte
    def b0(b: Byte) = new ShortPack(((s&0xFF00)|(b&0xFF)).toShort)
    def b1 = ((s&0xFFFF)>>8).toByte
    def b1(b: Byte) = new ShortPack(((s&0xFF)|((b&0xFF)<<8)).toShort)
    def swapB = new ShortPack((((s&0xFF00)>>8) | ((s&0xFF)<<8)).toShort)
    
    def h0 = unpackHalf(s)
    def h0(f: Float) = new ShortPack(packHalf(f))
  }
  implicit final class PackBytesInShort(val b: Byte) extends AnyVal {
    def packBB(c: Byte) = new ShortPack(((b&0xFF) | ((c&0xFF)<<8)).toShort)
  }
  implicit final class PackHalfInShort(val f: Float) extends AnyVal {
    def packH = new ShortPack(packHalf(f))
  }
  implicit final class ShortAsPacked(val s: Short) extends AnyVal {
    def packS = new ShortPack(s)
  }
  
  final class IntPack(val repr: Int) extends AnyVal {
    @inline private def i = repr
    def b0 = (i&0xFF).toByte
    def b0(b: Byte) = new IntPack((i&0xFFFFFF00) | (b&0xFF))
    def b1 = ((i&0xFF00)>>8).toByte
    def b1(b: Byte) = new IntPack((i&0xFFFF00FF) | ((b&0xFF)<<8))
    def b2 = ((i&0xFF0000)>>16).toByte
    def b2(b: Byte) = new IntPack((i&0xFF00FFFF) | ((b&0xFF)<<16))
    def b3 = (i>>>24).toByte
    def b3(b: Byte) = new IntPack((i&0xFFFFFF) | ((b&0xFF)<<24))
    def rotrB = new IntPack((i >>> 8) | (i << 24))
    def rotlB = new IntPack((i >>> 24) | (i << 8))
    def swap2B = new IntPack(((i&0xFF00FF00)>>>8) | ((i&0x00FF00FF)<<8))
    def flipB = new IntPack(((i&0xFF000000)>>24) | ((i&0xFF0000)>>8) | ((i&0xFF00)<<8) | ((i&0xFF)<<24))

    def s0 = (i & 0xFFFF).toShort
    def s0(s: Short) = new IntPack((i&0xFFFF0000) | (s&0xFFFF))
    def s1 = (i>>>16).toShort
    def s1(s: Short) = new IntPack((i&0xFFFF) | ((s&0xFFFF)<<16))
    def swapS = new IntPack((i>>>16) | (i<<16))

    def h0 = unpackHalf(s0)
    def h0(h: Float) = s0(packHalf(h))
    def h1 = unpackHalf(s1)
    def h1(h: Float) = s1(packHalf(h))
    def negateH = new IntPack(i ^ 0x80008000)

    def f0 = java.lang.Float.intBitsToFloat(i)
    def f0(f: Float) = java.lang.Float.floatToRawIntBits(f)
  }
  implicit final class PackBytesInInt(val b: Byte) extends AnyVal {
    def packBBBB(c: Byte, d: Byte, e: Byte) = new IntPack((b&0xFF) | ((c&0xFF)<<8) | ((d&0xFF)<<16) | (e.toInt<<24))
  }
  implicit final class PackShortsInInt(val s: Short) extends AnyVal {
    def packSS(t: Short) = new IntPack((s&0xFFFF) | (t.toInt << 16))
  }
  implicit final class PackHalvesInInt(val f: Float) extends AnyVal {
    def packHH(g: Float) = packHalf(f) packSS packHalf(g)
  }
  implicit final class PackFloatInInt(val f: Float) extends AnyVal {
    def packF = new IntPack(java.lang.Float.floatToRawIntBits(f))
  }
  implicit final class IntAsPacked(val i: Int) extends AnyVal {
    def packI = new IntPack(i)
  }
  
  final class LongPack(val repr: Long) extends AnyVal {
    @inline private def l = repr
    def b0 = (l & 0xFFL).toByte
    def b0(b: Byte) = new LongPack((l&0xFFFFFFFFFFFFFF00L) | (b&0xFF))
    def b1 = ((l&0xFF00)>>8).toByte
    def b1(b: Byte) = new LongPack((l&0xFFFFFFFFFFFF00FFL) | ((b&0xFF)<<8))
    def b2 = ((l&0xFF0000)>>16).toByte
    def b2(b: Byte) = new LongPack((l&0xFFFFFFFFFF00FFFFL) | ((b&0xFF)<<16))
    def b3 = ((l&0xFF000000)>>24).toByte
    def b3(b: Byte) = new LongPack((l&0xFFFFFFFF00FFFFFFL) | ((b&0xFF).toLong<<24))
    def b4 = ((l&0xFF00000000L)>>32).toByte
    def b4(b: Byte) = new LongPack((l&0xFFFFFF00FFFFFFFFL) | ((b&0xFF).toLong<<32))
    def b5 = ((l&0xFF0000000000L)>>40).toByte
    def b5(b: Byte) = new LongPack((l&0xFFFF00FFFFFFFFFFL) | ((b&0xFF).toLong<<40))
    def b6 = ((l&0xFF000000000000L)>>48).toByte
    def b6(b: Byte) = new LongPack((l&0xFF00FFFFFFFFFFFFL) | ((b&0xFF).toLong<<48))
    def b7 = (l>>>56).toByte
    def b7(b: Byte) = new LongPack((l&0x00FFFFFFFFFFFFFFL) | ((b&0xFF).toLong<<56))
    def rotrB = new LongPack((l >>> 8) | (l << 56))
    def rotlB = new LongPack((l >>> 56) | (l << 8))
    def swap4B = new LongPack(((l&0xFF00FF00FF00FF00L)>>>8) | ((l&0x00FF00FF00FF00FFL)<<8))
    def flipB = {
      val m = swap4B.repr
      new LongPack((m>>>48) | ((m&0xFFFF00000000L)>>16) | ((m&0xFFFF0000L)<<16) | (m<<48))
    }

    def s0 = (l & 0xFFFFL).toShort
    def s0(s: Short) = new LongPack((l&0xFFFFFFFFFFFF0000L) | (s&0xFFFF))
    def s1 = ((l&0xFFFF0000)>>16).toShort
    def s1(s: Short) = new LongPack((l&0xFFFFFFFF0000FFFFL) | ((s&0xFFFF).toLong << 16))
    def s2 = ((l&0xFFFF00000000L)>>32).toShort
    def s2(s: Short) = new LongPack((l&0xFFFF0000FFFFFFFFL) | ((s&0xFFFF).toLong << 32))
    def s3 = (l>>>48).toShort
    def s3(s: Short) = new LongPack((l&0x0000FFFFFFFFFFFFL) | (s.toLong << 48))
    def rotrS = new LongPack((l >>> 16) | (l << 48))
    def rotlS = new LongPack((l >>> 48) | (l << 16))
    def swap2S = new LongPack(((l&0xFFFF0000FFFF0000L)>>>16) | ((l&0x0000FFFF0000FFFFL)<<16))
    def flipS = new LongPack((l>>>48) | ((l&0xFFFF00000000L)>>16) | ((l&0xFFFF0000L)<<16) | (l<<48))

    def h0 = unpackHalf(s0)
    def h0(h: Float) = s0(packHalf(h))
    def h1 = unpackHalf(s1)
    def h1(h: Float) = s1(packHalf(h))
    def h2 = unpackHalf(s2)
    def h2(h: Float) = s2(packHalf(h))
    def h3 = unpackHalf(s3)
    def h3(h: Float) = s3(packHalf(h))
    def negateH = new LongPack(l ^ 0x8000800080008000L)
    def rotrH = rotrS
    def rotlH = rotlS
    def swap2H = swap2S
    def flipH = flipS
    
    def i0 = (l & 0xFFFFFFFFL).toInt
    def i0(i: Int) = new LongPack((l&0xFFFFFFFF00000000L) | (i&0xFFFFFFFFL))
    def i1 = (l >>> 32).toInt
    def i1(i: Int) = new LongPack((l&0xFFFFFFFFL) | (i.toLong<<32))
    def swapI = new LongPack((l >>> 32) | (l << 32))  
    
    def f0 = java.lang.Float.intBitsToFloat(i0)
    def f0(f: Float) = i0(java.lang.Float.floatToRawIntBits(f))
    def f1 = java.lang.Float.intBitsToFloat(i1)
    def f1(f: Float) = i1(java.lang.Float.floatToRawIntBits(f))
    def swapF = swapI
    
    def d0 = java.lang.Double.longBitsToDouble(l)
    def d0(d: Double) = new LongPack(java.lang.Double.doubleToRawLongBits(d))
  }
  implicit final class PackBytesInLong(val b: Byte) extends AnyVal {
    def packBBBBBBBB(c: Byte, d: Byte, e: Byte, f: Byte, g: Byte, h: Byte, i: Byte) = {
      new LongPack((b&0xFFL) | ((c&0xFFL)<<8) | ((d&0xFFL)<<16) | ((e&0xFFL)<<24) | ((f&0xFFL)<<32) | ((g&0xFFL)<<40) | ((h&0xFFL)<<48) | (i.toLong<<56))
    }
  }
  implicit final class PackShortsInLong(val s: Short) extends AnyVal {
    def packSSSS(t: Short, u: Short, v: Short) = new LongPack((s&0xFFFFL) | ((t&0xFFFFL)<<16) | ((u&0xFFFFL)<<32) | (v.toLong<<48))
  }
  implicit final class PackHalvesInLong(val h: Float) extends AnyVal {
    def packHHHH(i: Float, j: Float, k: Float) = new LongPack( (packHalf(h) packSSSS( packHalf(i), packHalf(j), packHalf(k) )).repr )
  }
  implicit final class PackIntsInLong(val i: Int) extends AnyVal {
    def packII(j: Int) = new LongPack((i&0xFFFFFFFFL) | (j.toLong << 32))
  }
  implicit final class PackFloatsInLong(val f: Float) extends AnyVal {
    def packF0 = new LongPack(java.lang.Float.floatToRawIntBits(f).toLong & 0xFFFFFFFFL)
    def pack0F = new LongPack(java.lang.Float.floatToRawIntBits(f).toLong << 32)
    def packFF(g: Float) = new LongPack((java.lang.Float.floatToRawIntBits(f).toLong & 0xFFFFFFFFL) | (java.lang.Float.floatToRawIntBits(g).toLong << 32))
  }
  implicit final class PackDoubleInLong(val d: Double) extends AnyVal {
    def packD = new LongPack(java.lang.Double.doubleToRawLongBits(d))
  }
  implicit final class LongAsPacked(val l: Long) extends AnyVal {
    def packed = new LongPack(l)
  }
  
  implicit final class IntToRationalLong(val i: Int) extends AnyVal {
    def asNumerator = new LongAsRational((i packII 1).repr)
    def asDenominator = new LongAsRational((1 packII i).repr)
    def over(j: Int) = LongAsRational.reduce(i,j)
    def +(r: LongAsRational) = r + i
    def -(r: LongAsRational) = LongAsRational.reduce(i.toLong*r.den - r.num, r.den)
    def *(r: LongAsRational) = r * i
    def /(r: LongAsRational) = LongAsRational.reduce(i.toLong*r.den, r.num)
  }
  final class LongAsRational(val repr: Long) extends AnyVal {
    def num: Int = repr.packed.i0
    def num(i: Int): LongAsRational = LongAsRational.reduce(i,den)
    def den: Int = repr.packed.i1
    def den(j: Int): LongAsRational = LongAsRational.reduce(num,j)
    def unary_- = new LongAsRational(repr.packed.i0(-num).repr)
    def +(i: Int) = LongAsRational.reduce(num+i.toLong*den, den)
    def +(r: LongAsRational) = LongAsRational.reduce(num.toLong*r.den + den.toLong*r.num, den.toLong*r.den)
    def -(i: Int) = LongAsRational.reduce(num-i.toLong*den, den)
    def -(r: LongAsRational) = LongAsRational.reduce(num.toLong*r.den - den.toLong*r.num, den.toLong*r.den)
    def *(i: Int) = LongAsRational.reduce(i.toLong*num, den)
    def *(r: LongAsRational) = LongAsRational.reduce(num.toLong * r.num, den.toLong * r.den)
    def /(i: Int) = LongAsRational.reduce(i.toLong * num, den)
    def /(r: LongAsRational) = LongAsRational.reduce(num.toLong * r.den, den.toLong * r.num)
    def ===(r: LongAsRational) = (num.toLong*r.den == den.toLong*r.num)
    override def toString = num.toString+"/"+den.toString
    def toInt = num/den
    def toFloat = (num.toDouble/den).toDouble
    def toDouble = num.toDouble/den
  }
  object LongAsRational {
    def from(l: Long) = new LongAsRational(l)
    def from(f: Float): LongAsRational = {
      val i = math.floor(f).toInt
      val x = f-i
      if (x==0) i.asNumerator else {
        val ii = math.abs(i.toLong)*4
        var a = 0
        var b,c,d = 1
        while ({ var m = math.max(b,d); (m < 0x400000 && m*ii < Int.MaxValue)}) {
          val med = (a+c).toFloat/(b+d)
          if (x==med) return reduce(i*(b+d)+a+c, b+d)
          else if (x > med) { a += c; b += d }
          else { c += a; d += b }
        }
        if (math.abs(a.toFloat/b - x) < math.abs(c.toFloat/d)) reduce(i*b+a, b)
        else reduce(i*d+c, d)
      }
    }
    def apply(i: Int, j: Int) = new LongAsRational((i packII j).repr)
    def reduce(i: Long, j: Long) = {
      if (j < 0) {
        val k = gcd(-i, -j)
        if (k>1) new LongAsRational(((-i/k).toInt packII (-j/k).toInt).repr)
        else new LongAsRational(((-i).toInt packII (-j).toInt).repr)
      }
      else {
        val k = gcd(i,j)
        if (k>1) new LongAsRational(((i/k).toInt packII (j/k).toInt).repr)
        else new LongAsRational((i.toInt packII j.toInt).repr)
      }
    }
    def gcd(i: Long, j: Long): Long = {
      @tailrec def GCD(n: Long, m: Long): Long = {
        val k = n % m
        if (k==0) m else GCD(m, k)
      }
      if (i == Long.MinValue || j == Long.MinValue) {
        var k = 1
        var ii = i
        var jj = j
        while ((ii&0x1)==0 && (jj&0x1)==0) { k = k<<1; ii>>=1; jj>>=1 }
        k
      }
      else {
        val ii = math.abs(i)
        val jj = math.abs(j)
        if (jj > ii) GCD(jj,ii) else GCD(ii,jj)
      }
    }
    def lcm(i: Long, j: Long) = math.abs(i)*(math.abs(j)/gcd(i,j))
  }
  
  implicit final class FloatToComplexLong(val f: Float) extends AnyVal {
    def asReal = new LongAsComplex(f.packF0.repr)
    def asImaginary = new LongAsComplex(f.pack0F.repr)
    def I = new LongAsComplex(f.pack0F.repr)
    def reIm(g: Float) = new LongAsComplex((f packFF g).repr)
    def CiS(g: Float) = new LongAsComplex(((f*math.cos(g)).toFloat packFF (f*math.sin(g)).toFloat).repr)
    def +(c: LongAsComplex) = c + f
    def -(c: LongAsComplex) = new LongAsComplex((f-c.re packFF -c.im).repr)
    def *(c: LongAsComplex) = c * f
    def /(c: LongAsComplex) = c.reciprocal * f
  }
  final class LongAsComplex(val repr: Long) extends AnyVal {
    def re = repr.packed.f0
    def re(f: Float) = new LongAsComplex(repr.packed.f0(f).repr)
    def im = repr.packed.f1
    def im(f: Float) = new LongAsComplex(repr.packed.f1(f).repr)
    def unary_~ = new LongAsComplex(repr ^ 0x8000000000000000L)
    def unary_- = new LongAsComplex(repr ^ 0x8000000080000000L)
    def reciprocal = {
      val lr = re
      val li = im
      val iL2 = 1/(lr*lr + li*li)
      (lr*iL2) reIm (-li*iL2)
    }
    def R2 = {
      val lr = re
      val li = im
      lr*lr + li*li
    }
    def abs = math.sqrt(R2).toFloat
    def arg = math.atan2(im,re).toFloat
    def nan = java.lang.Float.isNaN(re) || java.lang.Float.isNaN(im)
    def +(f: Float) = re(re + f)
    def +(r: Float, i: Float) = (r+re) reIm (i+im)
    def -(f: Float) = re(re - f)
    def *(f: Float) = (re*f) reIm (im*f)
    def /(f: Float) = (re/f) reIm (im/f)
    def +(c: LongAsComplex) = (re+c.re) reIm (im+c.im)
    def -(c: LongAsComplex) = (re-c.re) reIm (im-c.im)
    def *(c: LongAsComplex) = {
      val lr = re
      val li = im
      val mr = c.re
      val mi = c.im
      (lr*mr - li*mi) reIm (lr*mi + li*mr)
    }
    def /(c: LongAsComplex) = {
      val lr = re
      val li = im
      val mr = c.re
      val mi = c.im
      val iL2 = 1/(mr*mr + mi*mi)
     ((lr*mr+li*mi)*iL2) reIm ((mr*li - lr*mi)*iL2)
    }
    def sqrt(positive: Boolean = true) = {
      val r = abs
      val lr = re
      val li = im
      val p = math.sqrt(math.max(0f,0.5f*(lr + r))).toFloat
      val q = (math.signum(li)*math.sqrt(math.max(0f,((r-lr)*0.5f)))).toFloat
      if (positive) p reIm q else -q reIm -p
    }
    def pow(n: Int) = (math.pow(abs,n).toFloat CiS (n*arg).toFloat)
    def root(n: Int, branch: Int = 0) = {
      val in = 1.0/n
      (math.pow(abs,in).toFloat CiS (in*(arg + 2*branch*math.Pi)).toFloat)
    }
    def ===(c: LongAsComplex) = (re==c.re && im==c.im)
    override def toString = (re + " + " + im + "i")
  }
  
  implicit final class FloatToPropErrLong(val f: Float) extends AnyVal {
    def asValue = new LongAsPropErr(f.packF0.repr)
    def asError = new LongAsPropErr(f.pack0F.repr)
    def +-(g: Float) = new LongAsPropErr((f packFF g).repr)
  }
  final class LongAsPropErr(val repr: Long) extends AnyVal {
    import PropErr._
    def value = repr.packed.f0
    def value(f: Float) = new LongAsPropErr(repr.packed.f0(f).repr)
    def error = repr.packed.f1
    def error(f: Float) = new LongAsPropErr(repr.packed.f1(f).repr)
    def unary_- = new LongAsPropErr(repr ^ 0x80000000L)
    def reciprocal = {
      val ix = 1f/value
      (ix +- error*ix*ix)
    }
    def nan = java.lang.Float.isNaN(value) || java.lang.Float.isNaN(error)
    def +(f: Float) = value(value+f)
    def +(f: Float, sd: Float) = (f+value) +- L2(sd, error)
    def +(e: LongAsPropErr) = (value + e.value) +- L2(error, e.error)
    def -(f: Float) = value(value-f)
    def -(f: Float, sd: Float) = (value-f) +- L2(sd, error)
    def -(e: LongAsPropErr) = (value - e.value) +- L2(error, e.error)
    def *(f: Float) = value*f +- error*f
    def *(f: Float, sd: Float) = { val v = value; f*v +- L2(v*sd, f*error) }
    def *(e: LongAsPropErr) = { val v = value; val w = e.value; v*w +- L2(v*e.error, w*error) }
    def /(f: Float) = value/f +- error/f
    def /(f: Float, sd: Float) = {
      val iy = 1.0f/f
      val xiy = value*iy
      xiy +- L2(error*iy, sd*xiy*iy)
    }
    def /(e: LongAsPropErr) = {
      val iy = 1.0f/e.value
      val xiy = value*iy
      xiy +- L2(error*iy, e.error*xiy*iy)
    }
    def sqrt = {
      val x = math.sqrt(value).toFloat
      x +- error*0.5f/x
    }
    def sq = { val x = value; x*x +- 2f*x*error }
    def exp = { val x = math.exp(value); val e = error; x.toFloat +- (if (math.abs(e)<1f) x*(e+0.5f*e*(e+0.3333333333f*e*e)) else x*(math.exp(e)-1)).toFloat }
    def log = { val x = value; val q = error/x; math.log(x).toFloat +- (if (q<0.5f) q+0.5f*q*(q+0.66666666667f*q*q) else math.log(1-q).toFloat) }
    def sin = { val v = value; val s = math.sin(v).toFloat; val c = math.cos(v).toFloat; val e = error; s +- e*(math.abs(c) + 0.5f*math.abs(s)*e) }
    def cos = { val v = value; val c = math.cos(v).toFloat; val s = math.sin(v).toFloat; val e = error; c +- e*(math.abs(s) + 0.5f*math.abs(c)*e) }
    def ===(e: LongAsPropErr) = (value==e.value && error==e.error)
    override def toString = (value + " +- " + error)
  }
  object PropErr {
    def L2(x: Float, y: Float) = math.sqrt(x*x + y*y).toFloat
  }
  
  implicit final class FloatToCoordLong(val f: Float) extends AnyVal {
    def asX = new Vff(f.packF0.repr)
    def asY = new Vff(f.pack0F.repr)
    def vff(g: Float) = new Vff((f packFF g).repr)
    def rtheta(g: Float) = PolarVff(f,g)
    def +(v: Vff) = v + f
    def -(v: Vff) = new Vff(((f-v.x) packFF (f-v.y)).repr)
    def *(v: Vff) = v * f
  }
  final class Vff(val repr: Long) extends AnyVal {
    def x = repr.packed.f0
    def x(f: Float) = new Vff(repr.packed.f0(f).repr)
    def y = repr.packed.f1
    def y(f: Float) = new Vff(repr.packed.f1(f).repr)
    def nan = (java.lang.Float.isNaN(x) || java.lang.Float.isNaN(y))
    def lenSq = { val a = x; val b = y; a*a + b*b }
    def len = math.sqrt(lenSq).toFloat
    def theta = math.atan2(y,x).toFloat
    def unary_- = (-x vff -y)
    def ^() = { val il = 1.0f/len; (x*il vff y*il) }
    def cw = (y vff -x)
    def ccw = (-y vff x)
    def +(f: Float) = (x+f vff y+f)
    def +(f: Float, g: Float) = (x+f vff y+g)
    def +(v: Vff) = (x+v.x vff y+v.y)
    def -(f: Float) = (x-f vff y-f)
    def -(f: Float, g: Float) = (x-f vff y-g)
    def -(v: Vff) = (x-v.x vff y-v.y)
    def *(f: Float) = (x*f vff y*f)
    def *(f: Float, g: Float) = x*f + y*g
    def *(v: Vff) = x*v.x + y*v.y
    def X(f: Float, g: Float) = x*g - y*f
    def X(v: Vff) = x*v.y - y*v.x
    def proj(f: Float, g: Float) = { val a = x; val b = y; val e = (a*f + b*g)/(f*f + g*g); (f*e vff g*e) }
    def proj(v: Vff) = { val a = x; val b = y; val c = v.x; val d = v.y; val e = (a*c + b*d)/(c*c + d*d); (c*e vff d*e) }
    def orth(f: Float, g: Float) = { val a = x; val b = y; val e = (a*f + b*g)/(f*f + g*g); (a-f*e vff b-g*e) }
    def orth(v: Vff) = { val a = x; val b = y; val c = v.x; val d = v.y; val e = (a*c + b*d)/(c*c + d*d); (a-c*e vff b-d*e) }
    def *^(f: Float, g: Float) = { val a = x; val b = y; ((a*f + b*g)/math.sqrt((a*a + b*b)*(f*f + g*g))).toFloat }
    def *^(v: Vff) = { val a = x; val b = y; val c = v.x; val d = v.y; ((a*c + b*d)/math.sqrt((a*a + b*b)*(c*c + d*d))).toFloat }
    def distSq(f: Float, g: Float) = { val a = x-f; val b = y-g; a*a + b*b }
    def distSq(v: Vff) = { val a = x-v.x; val b = y-v.y; a*a + b*b }
    def dist(f: Float, g: Float) = math.sqrt(distSq(f,g)).toFloat
    def dist(v: Vff) = math.sqrt(distSq(v)).toFloat
    def angle(f: Float, g: Float) = { val a = x; val b = y; (math.acos(math.max(-1,math.min(1,((a*f+b*g)/math.sqrt((a*a + b*b)*(f*f + g*g))))))*math.signum(a*g-b*f)).toFloat }
    def angle(v: Vff) = { val a = x; val b = y; val c = v.x; val d = v.y; (math.acos(math.max(-1,math.min(1,((a*c+b*d)/math.sqrt((a*a + b*b)*(c*c + d*d))))))*math.signum(a*d-b*c)).toFloat }
    def ===(v: Vff) = (x == v.x) && (y == v.y)
    override def toString = "["+x+", "+y+"]"
    def polar = PolarVff(len, theta)
    def toTuple = (x, y)
    def toPoint = new java.awt.Point(math.round(x), math.round(y))
    def toPoint2D = new java.awt.geom.Point2D.Float(x, y)
    def toDimension = new java.awt.Dimension(math.round(x), math.round(y))
  }
  object Vff {
    val NaN = Float.NaN vff Float.NaN
    def from(l: Long) = new Vff(l)
    def from(d: Double, e: Double) = (d.toFloat vff e.toFloat)
    def from(t: (Double, Double)) = (t._1.toFloat vff t._2.toFloat)
    def apply(f: Float, g: Float) = (f vff g)
    def rtheta(f: Float, g: Float) = (f rtheta g).cartesian
    def apply(p: java.awt.geom.Point2D) = (p.getX.toFloat vff p.getY.toFloat)
    def apply(d: java.awt.geom.Dimension2D) = (d.getWidth.toFloat vff d.getHeight.toFloat)
    def apply(t: (Float, Float)) = (t._1 vff t._2)
  }
  
  final class PolarVff(val repr: Long) extends AnyVal {
    def r = repr.packed.f0
    def r(f: Float) = new PolarVff(repr.packed.f0(f).repr)
    def theta = repr.packed.f1
    def theta(f: Float) = new PolarVff(repr.packed.f1(f).repr)
    def nan = (java.lang.Float.isNaN(r) || java.lang.Float.isNaN(theta))
    def cartesian = Vff((r*math.cos(theta)).toFloat, (r*math.sin(theta)).toFloat)
  }
  object PolarVff {
    val NaN = new PolarVff((Float.NaN packFF Float.NaN).repr)
    def from(l: Long) = new PolarVff(l)
    def apply(r: Float, theta: Float) = new PolarVff((r packFF theta).repr)
  }
}
