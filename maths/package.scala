// ----------------------------------------------------------------------------
// ichi.maths contains mathematical and statistical routines for Scala
// Copyright (c) 2013, Rex Kerr and the Howard Hughes Medical Institute.
// All rights reserved.
// ichi.maths is provided as open source software under the BSD 3-clause license
// See http://www.opensource.org/licenses/bsd-license.php for text
// ----------------------------------------------------------------------------



package ichi

import scala.math._
import scala.reflect.ClassTag
import scala.annotation.tailrec

package object maths {

  abstract class Fn1D[@specialized A] {
    def apply(a: A): Double
  }
  implicit final val identityDoubleFn = new Fn1D[Double] { final def apply(d: Double) = d }
  implicit final val floatToDoubleFn = new Fn1D[Float] { final def apply(f: Float) = f }
  object Fn {
    def apply[@specialized A](f: A => Double) = new Fn1D[A] { final def apply(a: A) = f(a) }
  }
  
  // Useful constants computed to 20 digits using Mathematica 8.0
  final val OverSqrtTwo = 0.7071067811865475244
  final val OverSqrtTwoPi = 0.39894228040143267794
  final val SqrtTwoPi = 2.5066282746310005024
  final val SqrtTwo = 1.4142135623730950488
  final val OneOverPi = 0.31830988618379067154
  final val TwoOverPi = 0.63661977236758134308
  final val PiOverTwo = 1.5707963267948966192
  final val LnTwoPi = 1.8378770664093454836
  final val HalfLnTwoPi = 0.91893853320467274178
  final val GammaTwentyTwo = 5.109094217170944e19
  final val LanczosDoubleG = 6.0246800407767295837         // Magic value not from Mathematica
  final val SqrtTiniestDouble = sqrt(Double.MinPositiveValue)
  final val OverSqrtTiniestDouble = 1.0/SqrtTiniestDouble
  final val EpsDouble10x = 10*ulp(1.0)
  final val EpsDouble100x = 100*ulp(1.0)
  final val OverSqrtEight = 0.3535533905932737622
  final val QuarterSqrtPi = 0.44311346272637900682
  final val LnHalf = -0.69314718055994530942
  final val OverNinetyPctSigmas = 0.30397841595588447291   // One sigma is this fraction of the central 90% of a Gaussian distribution.
  
  // Useful conversions with constants selected such that 1.0 converted there and back remains 1.0 with standard IEEE 64 bit multiplication
  @inline final def fwhm2sigma(d: Double) = d*0.7413011092528010  // Conversion constant from Mathematica 8: 1/(2*InverseErf[1/2]*Sqrt[2])
  @inline final def sigma2fwhm(d: Double) = d*1.3489795003921635  // Conversion constant from Mathematica 8: 2*InverseErf[1/2]*Sqrt[2]


  // Infinities and Not-a-Numbers
  @inline final def isNaN(f: Float) = java.lang.Float.isNaN(f)
  @inline final def isNaN(d: Double) = java.lang.Double.isNaN(d)
  @inline final def isInf(f: Float) = java.lang.Float.isInfinite(f)
  @inline final def isInf(d: Double) = java.lang.Double.isInfinite(d)
  @inline final def isFinite(f: Float) = !(java.lang.Double.isNaN(f) || java.lang.Double.isInfinite(f))
  @inline final def isFinite(d: Double) = !(java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d))
  
  // Angles (in radians)
  @inline final def angleBetween(a: Float, b: Float) = { val c = a-b; c - 6.2831855f*math.rint(c*0.15915494f).toFloat }
  @inline final def angleBetween(a: Double, b: Double) = { val c = a-b; c - 2*math.Pi*math.rint(c*0.15915494309189535) }
  
  implicit class RichShortMaths(val value: Short) extends AnyVal {
    def bound(lo: Short, hi: Short) = math.min(hi, math.max(lo, value)).toShort
  }
  implicit class RichIntMaths(val value: Int) extends AnyVal {
    def bound(lo: Int, hi: Int) = math.min(hi, math.max(lo, value))
  }
  implicit class RichLongMaths(val value: Long) extends AnyVal {
    def bound(lo: Long, hi: Long) = math.min(hi, math.max(lo, value))
  }
  implicit class RichFloatMaths(val value: Float) extends AnyVal {
    def sq = value*value
    def sqrt = scala.math.sqrt(value)
    def sign = scala.math.signum(value)
    def ulp = scala.math.ulp(value)
    def nan = java.lang.Float.isNaN(value)
    def inf = java.lang.Float.isInfinite(value)
    def finite = !java.lang.Float.isNaN(value) && !java.lang.Float.isInfinite(value)
    def bits = java.lang.Float.floatToRawIntBits(value)
    def toDeg = value*57.29578f
    def toRad = value*0.017453292f
    def lower(f: Float) = if (f < value) f else if (value < f) value else if (java.lang.Float.isNaN(value)) f else value
    def upper(f: Float) = if (f > value) f else if (value > f) value else if (java.lang.Float.isNaN(value)) f else value
    def bound(lo: Float, hi: Float) = math.min(hi, math.max(lo, value))
    def angleNear(a: Float) = a + angleBetween(value, a)
  }
  implicit class RichDoubleMaths(val value: Double) extends AnyVal {
    def sq = value*value
    def sqrt = scala.math.sqrt(value)
    def sign = scala.math.signum(value)
    def ulp = scala.math.ulp(value)
    def nan = java.lang.Double.isNaN(value)
    def inf = java.lang.Double.isInfinite(value)
    def finite = !java.lang.Double.isNaN(value) && !java.lang.Double.isInfinite(value)
    def bits = java.lang.Double.doubleToRawLongBits(value)
    def toDeg = value*57.29577951308232
    def toRad = value*0.017453292519943295
    def lower(d: Double) = if (d < value) d else if (value < d) value else if (java.lang.Double.isNaN(value)) d else value
    def upper(d: Double) = if (d > value) d else if (value > d) value else if (java.lang.Double.isNaN(value)) d else value
    def bound(lo: Double, hi: Double) = math.min(hi, math.max(lo, value))
    def angleNear(a: Double) = a + angleBetween(value, a)
  }
  
  // Searching
  def indexSearch(a: Array[Int], i: Int, lteq: Boolean = true) = {
    var l=0
    var r=a.length-1
    while (l+1<r) {
      val m = (l+r)/2
      if (a(m) <= i) l=m
      else r=m
    }
    if (lteq || a(l)<=i) l else r
  }
  def indexSearchBy[A](a: Seq[A])(i: Int, i0: Int = 0, i1: Int = a.length, lteq: Boolean = true)(f: A => Int) = {
    var l = i0
    var r = i1-1
    while (l+1 < r) {
      val m = (l+r)/2
      if (f(a(m)) < i) l = m
      else r = m
    }
    if (lteq || f(a(l))<=i) l else r
  }
  def closestIndex(a: Array[Float], x: Float) = {
    var lo = 0
    var hi = a.length-1
    var i = (hi+lo) >> 1
    while (i != lo) {
      if (x < a(i)) hi = i else lo = i
      i = (hi+lo) >> 1
    }
    if (math.abs(x-a(lo)) <= math.abs(a(hi)-x)) lo else hi
  }
  def closestIndex(a: Array[Double], x: Double) = {
    var lo = 0
    var hi = a.length-1
    var i = (hi+lo) >> 1
    while (i != lo) {
      if (x < a(i)) hi = i else lo = i
      i = (hi+lo) >> 1
    }
    if (math.abs(x-a(lo)) <= math.abs(a(hi)-x)) lo else hi
  }
  def closestIndexBy[T](a: Array[T], x: Double)(f: T => Double) = {
    var lo = 0
    var hi = a.length-1
    var i = (hi+lo) >> 1
    while (i != lo) {
      if (x < f(a(i))) hi = i else lo = i
      i = (hi+lo) >> 1
    }
    if (math.abs(x-f(a(lo))) <= math.abs(f(a(hi))-x)) lo else hi
  }
  
  
  // Interpolation
  // Position of maximum; assumes center is the highest discretely sampled value (-1 = left, 0 = center, 1 = right)
  def quadraticInterpX(l: Double, c: Double, r: Double) = if (2*c-r-l == 0) 0 else 0.5*(r-l)/(2*c-r-l)
  def quadraticInterpX(l: Float, c: Float, r: Float) = if (2*c-r-l == 0) 0 else 0.5f*(r-l)/(2*c-r-l)
  // Value of maximum; assumes center is the highest discretely sampled value
  def quadraticInterpY(l: Double, c: Double, r: Double) = if (2*c-r-l==0) c else c + 0.25*(r-l)*(r-l)/(2*c-r-l)
  def quadraticInterpY(l: Float, c: Float, r: Float) = if (2*c-r-l==0) c else c + 0.25f*(r-l)*(r-l)/(2*c-r-l)
  // Value of function at particular X
  def quadraticInterp(l: Double, c: Double, r: Double, x: Double) = c + 0.5*x*(r - l + x*(r + l - 2*c))
  def quadraticInterp(l: Float, c: Float, r: Float, x: Float) = c + 0.5f*x*(r - l + x*(r + l - 2*c))

  
  // Gamma functions and their ilk (including complete beta)
  // Common operation when using Lanczos rational function approximation for gamma
  final def lanczosLogGTerm(x: Double) = (x-0.5)*log(x*0.36787944117144232160 + 2.0324162060519644573)
  // "Exact" (double precision) Lanczos rational function for gamma
  // Values taken from Boost 1.53, at 20 digits of precision for g value 6.024680040776729583740234375
  // Takes ~20 ns on 3.33 GHz Intel Xeon X5680
  final def lanczosApproximationRatio(x: Double) = {
    (56906521.9134715639 + x*(
      103794043.1163445452 + x*(
        86363131.28813859146 + x*(
          43338889.32467613834 + x*(
            14605578.08768506808 + x*(
              3481712.154980645909 + x*(
                601859.6171681098787 + x*(
                  75999.29304014542650 + x*(
                    6955.999602515376140 + x*(
                      449.9445569063168119 + x*(
                        19.51992788247617483 + x*(
                          0.5098416655656676188 + x*0.006061842346248906526))))))))))))/(
      x*(
        39916800 + x*(
          120543840 + x*(
            150917976 + x*(
              105258076 + x*(
                45995730 + x*(
                  13339535 + x*(
                    2637558 + x*(
                      357423 + x*(
                        32670 + x*(
                          1925 + x*(
                            66 + x)))))))))))   // Coefficients from Mathematica 8: CoefficientList[Product[(x + i), {i, 1, 11}], x]
    )
  }
  // Takes ~60 ns on 3.33GHz Intel Xeon X5680
  final def lnGamma(z: Double): Double = lanczosLogGTerm(z) + log(lanczosApproximationRatio(z))
  // Takes ~15-30 ns for integer z <= 21, 20-70 ns for integer 22 <= z <= 60, 110 ns for real z > 0, 210 ns for z < 0  (3.33GHz Intel Xeon X5680)
  final def gamma(z: Double): Double = {
    if (z > 0) {
      if (z <= 60.5 && abs(z-rint(z)) < 100*ulp(z)) {
        val n = math.round(z).toInt
        if (n <= 21) {
          var p = 1L
          var i = 2
          while (i < n) {
            p *= i
            i += 1
          }
          p.toDouble
        }
        else {
          var q = GammaTwentyTwo
          var i = 23
          while (i < n) {
            q *= i
            i += 1
          }
          q
        }
      }
      else exp(lanczosLogGTerm(z))*lanczosApproximationRatio(z)
    }
    else {
      val d = sin(Pi * z)
      if (d==0) Double.NaN else -Pi/(z*d*exp(lanczosLogGTerm(1-z))*lanczosApproximationRatio(1-z))
    }
  }
  final def lnGammaRat(z: Double, w: Double): Double = lanczosLogGTerm(z) - lanczosLogGTerm(w) + log(lanczosApproximationRatio(z)/lanczosApproximationRatio(w))
  final def gammaRat(z: Double, w: Double): Double = exp(lanczosLogGTerm(z) - lanczosLogGTerm(w))*lanczosApproximationRatio(z)/lanczosApproximationRatio(w)
  // lnBeta is lnGamma(a) + lnGamma(b) - lnGamma(a+b) but we'll take it apart to calculate more efficiently
  // Takes ~150 ns on a 3.33GHz Intel Xeon X5680
  final def lnBeta(a: Double, b: Double): Double = if (a < b) lnBeta(b,a) else if (a <= 0 || b <= 0) Double.NaN else {
    val c = a+b
    lanczosLogGTerm(a) + lanczosLogGTerm(b) - lanczosLogGTerm(c) + 
    log(lanczosApproximationRatio(a)*lanczosApproximationRatio(b)/lanczosApproximationRatio(c))
  }
  // beta is gamma(a+b)/(gamma(a)*gamma(b)) but we'll take it apart to calculate more efficiently
  // Takes 40-110 ns for small integer a,b, 200 ns for general case (large integer or real) (3.33GHz Intel Xeon X5680)
  final def beta(a: Double, b: Double): Double = if (a < b) beta(b,a) else if (a <= 0 || b <= 0) Double.NaN else {
    val c = a+b
    if (b < 40.5 && c < 1024.5 && abs(a-rint(a)) + abs(b-rint(b)) < 100*ulp(c)) {
      var n = round(c).toInt
      var m = round(b).toInt
      var p = 1.0
      var q = 1.0
      while (m >= 1) {
        p *= n
        q *= m
        m -= 1
        n -= 1
      }
      q/p
    }
    else {
      exp(lanczosLogGTerm(a)*lanczosLogGTerm(b)/lanczosLogGTerm(c)) *
      lanczosApproximationRatio(a)*lanczosApproximationRatio(b)/lanczosApproximationRatio(c)
    }
  }

  // Reasonably high-quality error/inverse error functions for general use
  // Based on Applied Statistics 37:477-484 (1988), alg. AS241
  // Takes ~45 ns on a 3.33 GHz Intel Xeon X5680
  def icdfNormal(p: Double) = {
    val h = p-0.5
    val x = 0.180625 - h*h
    if (x>=0) {
      h*
      (((((((2.5090809287301226727e3*x + 3.3430575583588128105e4
            )*x + 6.7265770927008700853e4
           )*x + 4.5921953931549871457e4
          )*x + 1.3731693765509461125e4
         )*x + 1.9715909503065514427e3
        )*x + 1.3314166789178437745e2
       )*x + 3.3871328727963666080e0
      ) /
      (((((((5.2264952788528545610e3*x + 2.8729085735721942674e4
            )*x + 3.9307895800092710610e4
           )*x + 2.1213794301586595867e4
          )*x + 5.3941960214247511077e3
         )*x + 6.8718700749205790830e2
        )*x + 4.2313330701600911252e1
       )*x + 1.0e0
      )
    }
    else {
      val hh = (if (h<=0) -1.0 else 1.0)
      val y = (if (h<=0) p else 1.0-p)
      val z = math.sqrt(-math.log(y))
      if (z<=5.0) {
        val x = z - 1.6
        hh*
        (((((((7.74545014278341407640e-4*x + 2.27238449892691845833e-2
              )*x + 2.41780725177450611770e-1
             )*x + 1.27045825245236838258e0
            )*x + 3.64784832476320460504e0
           )*x + 5.76949722146069140550e0
          )*x + 4.63033784615654529590e0
         )*x + 1.42343711074968357734e0
        ) /
        (((((((1.05075007164441684324e-9*x + 5.47593808499534494600e-4
              )*x + 1.51986665636164571966e-2
             )*x + 1.48103976427480074590e-1
            )*x + 6.89767334985100004550e-1
           )*x + 1.67638483018380384940e0
          )*x + 2.05319162663775882187e0
         )*x + 1.0
        )
      }
      else {
        val x = z - 5.0
        hh*
        (((((((2.01033439929228813265e-7*x + 2.71155556874348757815e-5
              )*x + 1.24266094738807843860e-3
             )*x + 2.65321895265761230930e-2
            )*x + 2.96560571828504891230e-1
           )*x + 1.78482653991729133580e0
          )*x + 5.46378491116411436990e0
         )*x + 6.65790464350110377720e0
        ) /
        (((((((2.04426310338993978564e-15*x + 1.42151175831644588870e-7
              )*x + 1.84631831751005468180e-5
             )*x + 7.86869131145613259100e-4
            )*x + 1.48753612908506148525e-2
           )*x + 1.36929880922735805310e-1
          )*x + 5.99832206555887937690e-1
         )*x + 1.0
        );
      }
    }
  }
  def erfInv(x: Double) = OverSqrtTwo*icdfNormal(0.5+0.5*x)
  def erfcInv(x: Double) = OverSqrtTwo*icdfNormal(1.0-0.5*x)
  // Piecewise rational function approximation of CDF for Normal distribution (courtesy of Mathematica 7)
  // Should be full double precision
  // Takes ~100ns on a 3.33 GHz Intel Xeon X5680 for moderate values
  def cdfNormal(y: Double) = {
    if (y > 8.3) 1.0 else if (y < - 38.5) 0.0 else {
      val x = if (y<0) -y else y
      val f = {
        if (x < 3) math.exp(
          -0.5*x*x -
          (((((((-3.6271830621274548308e-6*x - 6.2054577195631746255e-5
                )*x + 0.0020555154846807655013
               )*x + 0.032099345474574417685
              )*x + 0.21504119632351847003
             )*x + 0.73055326515392090713
            )*x + 1.3812898842892215850
           )*x + 0.69314718055994526146
          ) /
          (((((((-5.8186829446354815108e-7*x - 2.2135273033157240657e-5
                )*x + 3.6576165145176352643e-4
               )*x + 0.0094667294072793799548
              )*x + 0.078740088812851505927
             )*x + 0.34723234319509102797
            )*x + 0.84167596702197143827
           )*x + 1.0
          )
        )
        else if (x < 16) (math.exp( -0.5*x*x ) *
          ((((((0.00118089255719362346624*x + 0.0136334301130162766315
               )*x + 0.086474160844062169269
              )*x + 0.33993667920309143168
             )*x + 0.86339167691367313008
            )*x + 1.3345326346191572297
           )*x + 1
          ) /
          (((((((0.0029600586715196076372*x + 0.034173941597530707646
                )*x + 0.21971862448906668587
               )*x + 0.88626919617829879773
              )*x + 2.3750320592403537542
             )*x + 4.1290652702771203918
            )*x + 4.2651316245967753927
           )*x + 1.9999244808870340017
          )
        )
        else {
          val f0 = math.exp(-0.5*x*x)*OverSqrtTwoPi
          val z = 1/(x*x)
          var g, sum = 1/x
          var i = -1
          while (i >= -20) { g *= i*z; sum += g; i -= 2 }
          f0 * sum
        }
      }
      if (y>0) 1.0-f else f
    }
  }
  def erf(x: Double) = 2.0*cdfNormal(SqrtTwo*x)-1.0
  def erfc(x: Double) = -2.0*cdfNormal(-SqrtTwo*x)
  
  // Student's T test distribution functions (special case of incomplete regularized beta)
  // Approximations from Hill, Comm. ACM, Algorithm 395 & 396, v13 pp 617-620 (1970)
  // Takes no more than about 180 ns on a 3.33 GHz Intel Xeon X5680 (df = 18)
  def cdfStudentT(df: Long, t0: Double): Double = {
    @tailrec def nestCosS(n: Int, ib: Double, x: Double): Double = if (n<4) x else nestCosS(n-2, ib, 1 + (x*ib*(n-3))/(n-2))
    val t = abs(t0)
    val p = {
      if (df == 1) 0.5*(1 - TwoOverPi*atan(t))
      else {
        val y = t*t/df
        if (df >= 20) {
          val dg = df - 0.5
          val b = 48*dg*dg
          val z = if (y > 1e-6) dg*log(1+y) else dg*y
          cdfNormal( -sqrt(z)*(((((-0.4*z - 3.3)*z - 24.0)*z - 85.5)/(0.8*z*z+100+b) + z + 3)/b + 1) )
        }
        else {
          val iy1 = 1/(1+y)
          val cs = if (df<4) 1.0 else nestCosS(df.toInt, iy1, 1.0)
          0.5*max(0 , 1 - (if ((df&1)==0) sqrt(y/(1+y))*cs else { var yrt = sqrt(y); TwoOverPi*(atan(yrt) + yrt*iy1*cs) }))
        }
      }
    }
    if (t0 < 0) p else 1-p
  }
  // Takes about 350 ns on a 3.33 GHz Intel Xeon X5680 (df = 12)
  def icdfStudentT(df: Long, p0: Double): Double = {
    val p = if (p0 > 0.5) 2*(1-p0) else 2*p0
    val t = {
      if (df < 2) 1.0/tan(p*PiOverTwo)
      else if (df == 2) sqrt(2/(p*(2-p)) - 2)
      else {
        val dg = df - 0.5
        val idg = 1/dg
        val b = 48*dg*dg
        val ib = 1/b
        val c = ((20700*idg*ib - 98)*idg-16)*idg + 96.36
        val d = ((94.5/(b+c)-3)*ib+1)*sqrt(idg*PiOverTwo)*df
        val y = math.pow(d*p, 2.0/df)
        val z = {
          if (y > 0.05 + idg) {
            val in = icdfNormal(p*0.5)
            val insq = in*in
            val e = if (df < 5) c + 0.3*(df - 4.5)*(in+0.6) else c
            val f = (((0.05*d*in-5)*in-7)*in-2)*in + b + e
            val g = (((((0.4*insq + 6.3)*insq + 36)*insq + 94.5)/f - insq - 3)*ib + 1)*in
            val h = idg*g*g
            (if (h > 0.002) exp(h)-1 else 0.5*h*h+h)
          }
          else ((1/(((df + 6)/(df*y) - 0.089*d - 0.822)*(df+2)*3) + 0.5/(df+4))*y-1)*(df+1)/(df+2.0) + 1/y
        }
        sqrt(df*z)
      }
    }
    if (p0>0.5) t else -t
  }
  
  // Regularized incomplete gamma functions and chi squared distributions
  // $\gamma (s,x) = \frac{1}{\Gamma (s)} \cdot \int_{0}^{x} t^{s-1} e^{-t} dt$
  // Using standard form found in Cuyt & Peterson's "Handbook of Continued Fractions for Special Functions"
  // unless x is small so the series form should do better.  Assumes s>0,x>0.
  // A better split could be found for s,x >> 1000
  final def igammaLowerTaylorTerm(s: Double, x: Double): Double = {
    var taylor = 1.0/s;
    var sum = taylor;
    var denom = 1.0+s
    while (taylor > 100*ulp(sum)) {
      taylor *= x/denom
      sum += taylor
      denom += 1.0
    }
    sum
  }
  final def igammaUpperContFracTerm(s: Double, x: Double): Double = {
    var cont = x + 1.0 - s
    var lentzC = OverSqrtTiniestDouble
    var lentzD = (if (abs(cont) < SqrtTiniestDouble) OverSqrtTiniestDouble else 1.0/cont)
    var factor = 2.0
    var prod = lentzD
    var i = 1
    while (abs(factor-1) > EpsDouble100x) {
      val a = i*(s-i)
      cont += 2.0
      lentzC = cont + a/lentzC
      if (abs(lentzC) < SqrtTiniestDouble) lentzC = SqrtTiniestDouble*signum(lentzC)
      lentzD = cont + a*lentzD
      if (abs(lentzD) < SqrtTiniestDouble) lentzD = OverSqrtTiniestDouble*signum(lentzD) else lentzD = 1.0/lentzD
      factor = lentzC*lentzD
      prod *= factor
      i += 1
    }
    prod
  }
  final def igammaRegShapeApprox(s: Double, x: Double) = exp(-x + s*log(x) - lnGamma(s))
  // Takes about 300 ns on a 3.33 GHz Intel Xeon X5680
  def igammaRegL(s: Double, x: Double) = if (x < s+1) igammaLowerTaylorTerm(s,x)*igammaRegShapeApprox(s,x) else 1.0 - igammaUpperContFracTerm(s,x)*igammaRegShapeApprox(s,x)
  // Takes about 300 ns on a 3.33 GHz Intel Xeon X5680
  def igammaRegU(s: Double, x: Double) = if (x < s+1) 1.0 - igammaLowerTaylorTerm(s,x)*igammaRegShapeApprox(s,x) else igammaUpperContFracTerm(s,x)*igammaRegShapeApprox(s,x)
  // Runtime equal to igammaRegL
  def cdfChiSq(df: Double, chisq: Double) = igammaRegL(0.5*df, 0.5*chisq)
  
  // Incomplete beta functions and F distribution based on DiDonato & Morris, ACM Trans Math Soft v18 pp360-373 (1992)
  // Additional inspiration taken from bratio.f90 by DD & M, and Boost 1.53 implementation and documentation also based on DD & M
  def ddmMethodBPSER(a: Double, b: Double, x: Double) = {
    var nu = a
    var de = 1.0
    var term = EpsDouble100x
    var sum = 1.0
    var j = 0
    while (abs(term) >= sum*EpsDouble100x) {
      j += 1
      nu *= (j-b)*x
      de *= j
      term = nu/(de*(a+j))
      sum += term
    }
    exp(a * log(x) - lnBeta(a, b))*sum/a
  }
  def ddmMethodBUP(a: Double, b: Double, x: Double, n: Int) = {
    var term = exp(a*log(x) + b*log(1-x) - lnBeta(a,b))/a
    var sum = term
    var j = 1
    val ab1 = a+b-1
    val earliable = (if (b <= 1) 1 else ceil((b-1)*x/(1-x) - a).toInt)
    while (j < n && (j <= earliable || sum*EpsDouble100x < term)) {
      term *= (ab1+j)*x/(a+j)
      sum += term
      j += 1
    }
    sum
  }
  // Only gives about 9 digits accuracy
  def ddmMethodBGRAT(a: Double, b: Double, x: Double, w: Double = 0.0) = {
    val t = a + 0.5*(b-1)
    val lx = log(x)
    val u = -t*lx
    val lh = -u + b*log(u) - lnGamma(b)
    val m = exp(lh - b*log(t) + lnGammaRat(a+b,a))
    val ew = abs(w/m)*EpsDouble100x
    var p = new Array[Double](8); p(0) = 1
    val i4tsq = 1/(4*t*t)
    val lx2sq = 0.25*lx*lx
    var j = igammaRegU(b, u)*exp(-lh)
    var term = j
    var sum = term
    var n = 0
    val ub = u + b
    var q = b-1
    var g = 1.0
    while (max(ew,sum*EpsDouble100x) < abs(term)) {
      j = i4tsq*(g*(ub+(2*n+1)) + (b+2*n)*(b+2*n+1)*j)
      g *= lx2sq
      n += 1
      q /= 2*n*(2*n+1)
      var m = 1
      var s = 0.0
      var r = 1.0
      while (m < n) {
        r *= (m*b-n)/(2*m*(2*m+1))
        s += r*p(n-m)
        m += 1
      }
      if (n >= p.length) { val pp = new Array[Double](p.length*2); System.arraycopy(p,0,pp,0,p.length); p = pp }
      p(n) = q + s/n
      term = p(n)*j
      sum += term
    }
    m * sum
  }
  def ddmMethodBFRAC(a: Double, b: Double, x: Double) = {
    val lam1 = 1 + a - (a+b)*x
    val ia = 1/a
    var p = 1.0
    var an = 0.0
    var bn = 1.0
    var an1 = 1.0
    var bn1 = lam1/(1 + ia)
    var r = (1 + ia)/lam1
    var n = 1
    while (n != 0) {
      val w = n*(b - n)*x
      val ia2n1 = 1/(a + (2*n - 1))
      val e = a*ia2n1
      val alph = (p*(p + b*ia)*e*e)*(w*x)
      if (alph <= 0) n = 0
      else {
        p = 1 + n*ia
        val bet = n + w*ia2n1 + (p/(1+ia*(2*n+1)))*(lam1 + n*(2 - x))
        val aa = alph*an + bet*an1; an = an1; an1 = aa
        val bb = alph*bn + bet*bn1; bn = bn1; bn1 = bb
        val r0 = r
        val ibn1 = 1/bn1
        r = an1*ibn1
        if (abs(r-r0) <= EpsDouble100x*r) n = 0
        else {
          an *= ibn1
          an1 = r
          bn *= ibn1
          bn1 = 1
          n += 1
        }
      }
    }
    r * exp(a*log(x) + b*log(1-x) - lnBeta(a,b))
  }
  // Incomplete regularized beta.  At least 9 digits of accuracy almost everywhere.
  // ~1000 ns for most values, except for large a,b with x near a/(a+b), which takes ~2000*log10((a+b)/1000) ns (all on a 3.33 GHz Intel Xeon X5680)
  def ibetaReg(a: Double, b: Double)(x: Double): Double = {
    if (a <= 0 || b <= 0) return Double.NaN
    val y = 1-x
    if (x <= 0 || y <= 0) return (if (min(x,y) > -EpsDouble100x) { if (x < 0.5) 0 else 1 } else Double.NaN)
    val abm = min(a,b)
    val abM = max(a,b)
    lazy val lxa = a*log(x)
    lazy val lbxa = a*log(x*b)
    if (abm < 1) {
      if (x > 0.5) 1 - ibetaReg(b, a)(1-x)
      else if (abM <= 1) {
        if (a >= min(0.2,b) || (lxa <= -0.1053605156578263 /* log(0.9) */)) ddmMethodBPSER(a, b, x)
        else if (x >= 0.3) 1 - ddmMethodBPSER(b, a, 1-x)
        else { val w = ddmMethodBUP(b, a, 1-x, 20); 1 - (w + ddmMethodBGRAT(b + 20, a, 1-x, w)) }
      }
      else if (b <= 1) ddmMethodBPSER(a, b, x)
      else {
        if (x >= 0.3) 1 - ddmMethodBPSER(b, a, 1-x)
        else if (x < 0.1 && lbxa <= -0.35667494393873238 /* log(0.7) */) ddmMethodBPSER(a, b, x)
        else { val (n,w) = (if (b<=15) (20, ddmMethodBUP(b, a, 1-x, 20)) else (0, 0.0)); 1 - (w + ddmMethodBGRAT(b + n, a, 1-x, w)) }
      }
    }
    else if (x*(a+b) > a) 1 - ibetaReg(b, a)(1-x)
    else if (b >= 40) ddmMethodBFRAC(a, b, x)
    else {
      val m = ceil(b).toInt - 1
      if (b*x < 0.7) ddmMethodBPSER(a, b, x)
      else if (x <= 0.7) ddmMethodBUP(b-m, a, 1-x, m) + ddmMethodBPSER(a, b-m, x)
      else {
        val w = ddmMethodBUP(b-m, a, 1-x, m)
        val (n,v) = (if (a<=15) (20, ddmMethodBUP(a, b-m, x,20)) else (0, 0.0))
        w + v + ddmMethodBGRAT(a+n, b-m, x, w+v)
      }
    }
  }
  // F distribution from  incomplete regularized beta
  def cdfFDistribution(F: Double)(n: Int, m: Int) = ibetaReg(0.5*n, 0.5*m)(n*F/(n*F+m))
  
  trait StandardDeviation {
    def n: Int
    def mean: Double
    def sd: Double
    def fixed: StdDev = new StdDev(n, mean, sd)
    def scaled(f: Double): StdDev = new StdDev(n, mean*f, sd*f)
    def Ex = n*mean
    def Exx = if (n<=0) 0 else if (n==1) mean*mean else {
      val s = sd*n
      val x = Ex
      s*s/(n-1) + x*x
    }
    def variance = sd*sd
    def sem = sd/sqrt(max(n-1,1))
    def sumOfSquares = if (n<2) 0.0 else { val s = sd; (n-1)*s*s }
    def p(x: Double) = if (n<2) 0.5 else cdfNormal((x-mean)/sd)
    def tailP(x: Double) = max(0.0, min(1.0, 1-2*abs(p(x)-0.5)))
    def meanP(m: Double) = if (n<2) 0.5 else cdfNormal((m-mean)/sem)
    def meanTailP(m: Double) = max(0.0, min(1.0, 1-2*abs(meanP(m)-0.5)))
    def x(p: Double) = if (p<=0.0 || p>=1.0 || n==0) Double.NaN else icdfNormal(p)*sd + mean
    def mean(p: Double): Double = if (p<=0.0 || p>=1.0 || n==0) Double.NaN else icdfNormal(p)*sem + mean
    def tTest(sd2: StandardDeviation): Double = {
      val n2 = sd2.n
      val df = n + (n2 - 2L)
      if (df < 1 || n==0 || n2==0) 0.5
      else if (n==1) sd2.tTest(this)
      else {
        val diff = -abs(mean - sd2.mean)
        val devs: Double = {
          if (n2==1) sqrt(sumOfSquares*(n+1.0)/((n-1.0)*n))
          else sqrt(((sumOfSquares + sd2.sumOfSquares)/df)*(n.toDouble+n2)/(n.toDouble*n2))
        }
        2*cdfStudentT(df,diff/devs)
      }
    }
    def stringTag = ""
    override def toString = s"$mean +- $sd (n=$n)$stringTag"
  }
  case class StdDev(n: Int, mean: Double, sd: Double) extends StandardDeviation {
    override def fixed = this
    override lazy val sem = super.sem
  }
  object StdDev {
    val empty = StdDev(0, Double.NaN, Double.NaN)
    def finite[@specialized(Float, Double) A](a: Array[A], start: Int = 0, end: Int = -1)(implicit f: Fn1D[A]): StdDev = {
      val i0 = if (start < 0) a.length+start else start
      val i1 = if (end < 0) a.length+end+1 else end
      var m,k = 0.0
      var n = 0
      var i = i0
      while (i < i1) {
        val x = f(a(i))
        if (isFinite(x)) {
          val dx = x - m
          n += 1
          m += dx/n
          k += dx*(x-m)
        }
        i += 1
      }
      new StdDev(n, m, sqrt(k/max(n-1,1)))
    }
    def subrange[@specialized(Float, Double) A](a: Array[A], i0: Int, i1: Int)(implicit f: Fn1D[A]): StdDev = {
      var m,k = 0.0
      var n = 0
      var i = i0
      while (i < i1) {
        val x = f(a(i))
        val dx = x - m
        n += 1
        m += dx/n
        k += dx*(x-m)
        i += 1
      }
      new StdDev(n, m, sqrt(k/max(n-1,1)))
    }
    def apply[@specialized(Float,Double) A](a: Array[A])(implicit f: Fn1D[A]): StdDev = {
      var m,k = 0.0
      var n = 0
      while (n < a.length) {
        val x = f(a(n))
        val dx = x - m
        n += 1
        m += dx/n
        k += dx*(x - m)
      }
      new StdDev(n, m, sqrt(k/max(n-1,1)))
    }
    def bayes(i: Int, j: Int) = {
      val n = i+j
      new StdDev(n, (i+1.0)/(n+2.0), sqrt((i+1.0)*(j+1.0)/(n+3.0))/(n+2.0))
    }
    def fbayes(i: Float, j: Float) = {
      val n = i+j
      new StdDev(math.round(n), (i+1.0)/(n+2.0), sqrt((i+1.0)*(j+1.0)/(n+3.0))/(n+2.0))
    }
  }
  class AccumulateSD extends StandardDeviation with Cloneable {
    private var i = 0
    private var m = 0.0
    private var k = 0.0
    def +~(x: Double): this.type = {
      val dx = x - m
      i += 1
      m += dx/i
      k += dx*(x-m)
      this
    }
    def -~(x: Double): this.type = {
      val j = i
      i -= 1
      if (i==0) { m = 0; k = 0 }
      else {
        val n = m
        m = (j*m - x)/i
        k -= (x - m)*(x - n)
      }
      this
    }
    def ++~[@specialized(Float, Double) A](a: Array[A])(implicit f: Fn1D[A]): this.type = {
      var j = 0
      while (j<a.length) { this +~ f(a(j)); j += 1 }
      this
    }
    def --~[@specialized(Float, Double) A](a: Array[A])(implicit f: Fn1D[A]): this.type = {
      var j = 0
      while (j < a.length) {
        if (i==0) { m = 0.0; k = 0.0; return this }
        this -~ f(a(j));
        j += 1
      }
      this
    }
    def ++~(s: AccumulateSD): this.type = {
      if (s.i != 0) {
        if (i==0 ) {
          i = s.i
          m = s.m
          k = s.k
        }
        else if (s.i==1) this +~ s.m
        else {
          val oi = i
          val om = m
          i = oi + s.i
          m = (oi*om + s.i*s.m)/i
          val dm = om - s.m
          k += s.k + oi*(s.i*(dm*dm))/i
        }
      }
      this
    }
    def --~(s: AccumulateSD): this.type = {
      if (s.i != 0) {
        if (s.i >= i) {
          i = 0
          m = 0.0
          k = 0.0
        }
        else if (s.i+1 == i) {
          m = i*m - s.i*s.m
          i = 1
          k = 0.0
        }
        else {
          val oi = i
          val om = m
          i = oi - s.i
          m = (om*oi - s.i*s.m)/i
          val dm = (m - s.m)
          k -= s.k + i*(s.i*(dm*dm))/oi
        }
      }
      this
    }
    def ++~(sd: StandardDeviation): this.type = {
      if (sd.n != 0) {
        if (i==0) {
          i = sd.n
          m = sd.mean
          k = if (i==1) 0.0 else { val x = sd.sd; x*x*(i-1) }
        }
        else if (sd.n == 1) this +~ sd.mean
        else {
          val oi = i
          val om = m
          val si = sd.n
          val sm = sd.mean
          val sk = { val x = sd.sd; x*x*(si-1) }
          i = oi+si
          m = (oi*om + si*sm)/i
          val dm = om - sm
          k += sk + oi*(si*(dm*dm))/i
        }
      }
      this
    }
    def --~(sd: StandardDeviation): this.type = {
      if (sd.n != 0) {
        if (sd.n >= i) {
          i = 0
          m = 0.0
          k = 0.0
        }
        else if (sd.n+1 == i) {
          m = i*m - sd.n*sd.mean
          i = 1
          k = 0.0
        }
        else {
          val oi = i
          val om = m
          val si = sd.n
          val sm = sd.mean
          val sk = { val x = sd.sd; x*x*(si-1) }
          i = oi - si
          m = (om*oi - si*sm)/i
          val dm = (m - sm)
          k -= sk + i*(si*(dm*dm))/oi
        }
      }
      this
    }
    override def clone() = { val asd = new AccumulateSD; asd ++~ this; asd }
    def reset: this.type = { i=0; m=0.0; k=0.0; this }
    def n = i
    def mean = m
    def sd = sqrt(k/max(n-1,1))
    override def sumOfSquares = k
    override def stringTag = " accum."
  }
  object AccumulateSD {
    def apply() = new AccumulateSD
    def apply(d: Double*) = d match {
      case w: scala.collection.mutable.WrappedArray[_] => (new AccumulateSD) ++~ d.asInstanceOf[scala.collection.mutable.WrappedArray[Double]].array
      case _ => val asd = new AccumulateSD; d.foreach(asd +~ _); asd
    }
    def apply(asd: AccumulateSD) = (new AccumulateSD) ++~ asd
    def apply(sd: StandardDeviation) = (new AccumulateSD) ++~ sd
  }
  
  /** 
   * 2D linear fit in various forms; `a*x + b*y = c` is the standard form, and `theta`, `closest` is the angle and distance of closest approach to the origin
   * (normal form); `theta` is between -Pi/2 and Pi/2 so that positive distances are in the 0 to Pi direction.
   */
  trait LinearFit {
    def theta: Double
    def semTheta: Double
    def closest: Double
    def semClosest: Double
    def n: Int
    def vx: Double
    def vy: Double
    def cx: Double
    def cy: Double
    def a: Double
    def b: Double
    def c: Double
    def slope: Double
    def offset: Double
    def y(x: Double): Double = offset + x*slope
    def semY(x: Double): Double = { val xx = (x-cx)/vx; val e = semTheta; val ee = semClosest*abs(vx); sqrt(ee.sq + (e*(1+e*e*0.333333333333333333)*xx).sq) }
    def invslope: Double
    def invoffset: Double
    def x(y: Double): Double = invoffset + y*invslope
    def semX(y: Double): Double = { val yy = (y-cy)/vy; val e = semTheta; val ee = semClosest*abs(vy); sqrt(ee.sq + (e*(1+e*e*0.333333333333333333)*yy).sq) }
    def dist(x: Double, y: Double): Double = { val xx = x-cx; val yy = y-cy; yy*vx - xx*vy }
    def semDist(x: Double, y: Double): Double = { val xx = x-cx; val yy = y-cy; val d = vx*xx+vy*yy; val e = semTheta; sqrt(semClosest.sq + (e*(1+e*e*0.333333333333333333)*d).sq) }
    def fixed = LinFit(theta, semTheta, closest, semClosest, n)(vx, vy, cx, cy)
    def rmse[@specialized(Float, Double) A](xs: Array[A], ys: Array[A])(implicit f: Fn1D[A]) = {
      var s = 0.0
      var i = 0
      while (i< xs.length) {
        s += dist(f(xs(i)),f(ys(i))).sq
        i += 1
      }
      sqrt(s / max(1,xs.length))
    }
  }
  
  case class LinFit(theta: Double, semTheta: Double, closest: Double, semClosest: Double, n: Int)(vx0: Double = Double.NaN, vy0: Double = Double.NaN, cx0: Double = Double.NaN, cy0: Double = Double.NaN) extends LinearFit {
    val (vx, vy) = {
      if (isNaN(vx0) || isNaN(vy0)) (cos(theta), sin(theta))
      else if (abs(vx0*vx0 + vy0*vy0 - 1) < EpsDouble100x) (vx0, vy0)
      else { val iv = 1.0/math.sqrt(vx0*vx0 + vy0*vy0); (vx0*iv, vy0*iv) }
    }
    val (cx, cy) = {
      if (isNaN(cx0) || isNaN(cy0)) (-closest*vy, closest*vx)
      else {
        val pdot = -vy*(cx0+closest*vy) + vx*(cy0-closest*vx)
        if (abs(pdot) < max(1,max(abs(closest),max(abs(cx0),abs(cy0))))*EpsDouble100x) (cx0, cy0)
        else (cx0 + pdot*vy, cy0 - pdot*vx)
      }
    }
    val slope = vy/vx
    val offset = cy - vy*cx/vx
    val invslope = vx/vy
    val invoffset = cx - vx*cy/vy
    def a = if (abs(slope)>1) 1.0 else slope
    def b = if (abs(slope)>1) invslope else 1.0
    def c = if (closest==0) 0.0 else if (abs(slope)>1) offset else invoffset
    override def fixed = this
  }
  
  /*
  class AccumulateLin extends LinearFit with Cloneable {
    private var n = 0
    private var mx = 0.0
    private var my = 0.0
    private var sxx = 0.0
    private var sxy = 0.0
    private var syy = 0.0
    def +~(x: Double, y: Double): this.type = {
      n += 1
      val in = 1.0/n
      mx += in*(x-mx)
      my += in*(y-my)
      
      this
    }
  }
  */
  
  object Parametric {
    def tailP[@specialized(Float, Double) A](x: A, mean: A, sd: A)(implicit f: Fn1D[A]) = max(0.0, min(1.0, 1-2*abs(cdfNormal((f(x)-f(mean))/f(sd))-0.5)))
    def tailP[@specialized(Float, Double) A](a0: Array[A], a: A)(implicit f: Fn1D[A]) = StdDev(a0).tailP(f(a))
    def tTest[@specialized(Float, Double) A](a0: Array[A], a1: Array[A])(implicit f: Fn1D[A]) = StdDev(a0).tTest(StdDev(a1))
    def anova(sds0: Seq[StandardDeviation]): Double = {
      val sds = sds0.toArray
      var within = 0.0
      var avg = 0.0
      var i,n = 0
      while (i < sds.length) {
        val m = sds(i).n
        if (m<1) return Double.NaN
        within += sds(i).sumOfSquares
        avg += sds(i).mean*m
        n += m
        i += 1
      }
      if (sds.length == n) return Double.NaN
      avg /= n
      within /= n-sds.length
      var between = 0.0
      i = 0
      while (i < sds.length) {
        val dx = sds(i).mean - avg
        between += sds(i).n*dx*dx
        i += 1
      }
      between /= (sds.length - 1)
      max(0.0, min(1.0, 1-cdfFDistribution(between / within)(sds.length - 1, n - sds.length)))
    }
    def anova[@specialized(Float, Double) A](as: Array[A]*)(implicit f: Fn1D[A]): Double = if (as.length < 2) Double.NaN else anova(as.map(a => StdDev(a)(f)))
    def logLikelihoodRatio(df1: Int, logp1: Double, df2: Int, logp2: Double) = if (df1>=df2) Double.NaN else if (logp1>=logp2) 1.0 else cdfChiSq(2*(logp2 - logp1), df2 - df1)
    def likelihoodRatio(df1: Int, p1: Double, df2: Int, p2: Double) = if (df1>=df2) Double.NaN else if (p1>=p2) 1.0 else cdfChiSq(-2*log(p1/p2), df2-df1)
    // p value for F-based regression test that the large model with df2 parameters and rss2 error is statistically equivalent to the small model with df1, rss1
    def regressionTest(df1: Int, rss1: Double, df2: Int, rss2: Double, n: Int) = {
      if (df1 >= df2 || n <= df2) Double.NaN
      else max(0.0, min(1.0, 1 - cdfFDistribution( (rss1/rss2-1)*(n-df2)/(df2-df1) )(df2 - df1, n - df2)))
    }
  }
  object Nonparametric {
    def rankT[@specialized(Float,Double) A](a: Array[A], b: Array[A])(implicit f: Fn1D[A]): Double = {
      val c = new Array[Double](a.length + b.length)
      var i = 0
      var j = 0
      while (i < a.length) {
        val x = f(a(i))
        if (!isNaN(x)) {
          c(j) = x
          j += 1
        }
        i += 1
      }
      val aok = j
      i = 0
      while (i < b.length) {
        val x = f(b(i))
        if (!isNaN(x)) {
          c(j) = x
          j += 1
        }
        i += 1
      }
      val bok = j - aok
      if (aok==0 || bok==0 || aok+bok==1) 0.5
      else {
        val sa, sb = new AccumulateSD
        val clen = j
        java.util.Arrays.sort(c,0,j)
        val d = new Array[Double](clen)
        i = 0
        j = 0
        while (i < clen) {
          val x = c(i)
          j = i+1
          while (j < clen && c(j)==x) j += 1
          val y = 0.5*(i+j-1)
          var k = i
          while (k < j) { d(k) = y; k += 1 }
          i = j
        }
        i = 0
        while (i < aok) {
          val x = f(a(i))
          if (!isNaN(x)) {
            j = 0
            var k = clen-1
            while (k - j > 1) {
              val h = (k+j) >>> 1
              val y = c(h)
              if (x <= y) k = h
              if (x >= y) j = h
            }
            sa +~ (if (x == c(j)) d(j) else d(k))
          }
          i += 1
        }
        i = 0
        while (i < bok) {
          val x = f(b(i))
          if (!isNaN(x)) {
            j = 0
            var k = clen-1
            while (k - j > 1) {
              val h = (k+j) >>> 1
              val y = c(h)
              if (x <= y) k = h
              if (x >= y) j = h
            }
            sb +~ (if (x == c(j)) d(j) else d(k))
          }
          i += 1
        }
        sa.tTest(sb)
      }
    }
    
    /**
     * WARNING: there are bugs in the implementation of theilsen that yield inaccurate confidence intervals.
     */
    /*
    def theilsen[@specialized(Float, Double) A](x: Array[A], y: Array[A], sampling: Int = 4096, confidence: Boolean = true)(implicit f: Fn1D[A]): LinearFit = {
      if (x.length<5) throw new IllegalArgumentException("Nonparametric line fits must have at least five data points.")
      if (x.length!=y.length) throw new IllegalArgumentException(s"Data size mismatch in Theil-Sen line fit: ${x.length} vs ${y.length}")
      val nasked = max(x.length,max(if (confidence) 256 else 32, sampling))
      val nbits = if (confidence) round(sqrt(nasked*0.25)).toInt else 1
      val stride = if (confidence) nbits*4 else nasked
      val slopes = new Array[Double](stride*nbits)
      val select = new Array[Long](if (confidence) x.length else 0)
      val div = Long.MaxValue/x.length
      val reject = div*x.length
      var nz, ni, nn = new Array[Int](nbits)
      var rn = scala.util.Random.nextLong
      var block = 1
      @tailrec def nx(): Int = {
        val r = rn & Long.MaxValue
        rn = rn*2862933555777941757L + 3037000493L   // Mascagni SPRNG lcg64 values
        if (r < reject) (r/div).toInt else nx()
      }
      def rix(): Int = if (!confidence) nx() else {
        val k = nx()
        if (select(k)>>>32 != block) select(k) = (block.toLong << 32) | nx()
        select(k).toInt
      }
      var h = 0
      while (h < slopes.length) {
        if (block*stride < h) block += 1
        val i = rix()
        val j = rix()
        slopes(h) = {          
          if (i==j) { 
            nn(block-1) += 1
            Double.NaN
          }
          else {
            val dx = f(x(j)) - f(x(i))
            val dy = f(y(j)) - f(y(i))
            if (dx==0) {
              if (dy==0) {
                nn(block-1) += 1
                Double.NaN
              }
              else {
                ni(block-1) += 1
                dy/dx
              }
            }
            else if (dy==0) nz(block-1) += 1
            dy/dx
          }
        }
        h += 1
      }
      val ms = new Array[Double](nbits)
      var nv = 0
      h = 0
      while (h < ms.length) {
        val steep = ((ni(h) > 2*nz(h) && 100L*ni(h) > slopes.length) || (ni(h) > nz(h) && nn(h)+nz(h)+slopes.length/4 > slopes.length))
        val i0 = h*stride
        val i1 = i0+stride
        if (steep) {
          var i = i0
          val i1 = (h+1)*stride
          while (i < i1) { slopes(i) = 1.0/slopes(i); i += 1 }
        }
        val m = rankFractionD(slopes, 0.5, i0, i1)
        ms(h) = atan(if (steep) 1.0/m else m)
        if (abs(ms(h)) > PiOverTwo) nv += 1
        h += 1
      }
      val (theta, semTheta) = if (!confidence) (ms(0), 0.0) else {
        if (2*nv > ms.length) {
          var i = 0
          while (i < ms.length) {
            if (ms(i) < 0) ms(i) += Pi
            i += 1
          }
        }
        val m = StdDev(ms)
        (if (m.mean > PiOverTwo) m.mean-Pi else m.mean, m.sem)
      }
      
      val vx = -sin(theta)
      val vy = cos(theta)
      var i = 0
      while (i < x.length) { slopes(i) = vx*f(x(i)) + vy*f(y(i)); i += 1 }
      val closest = rankFractionD(slopes, 0.5, 0, x.length)
      val semClosest = {
        if (!confidence) 0.0
        else if (x.length < 40) fwhm2sigma( rankFractionD(slopes, 0.75, 0, x.length) - rankFractionD(slopes, 0.25, 0, x.length) )
        else OverNinetyPctSigmas * (rankFractionD(slopes, 0.95, 0, x.length) - rankFractionD(slopes, 0.05, 0, x.length))
      }/sqrt(max(1,x.length-1))
      if (!confidence) LinFit(theta, semTheta, closest, semClosest, x.length)(vy, -vx)
      else {
        val z = new Array[Double](2*x.length)
        var j,k = 0
        while (j < x.length) { val v = f(x(j)); if (!isNaN(v)) { z(k) = v; k += 1 }; j += 1 }
        val medx = if (k>0) rankIndexD(z, k/2, 0, k, k) else Double.NaN
        j = 0
        k = 0
        while (j < x.length) { val v = f(y(j)); if (!isNaN(v)) { z(k) = v; k += 1}; j += 1 }
        val medy = if (k>0) rankIndexD(z, k/2, 0, k, k) else Double.NaN
        LinFit(theta, semTheta, closest, semClosest, x.length)(vy, -vx, medx, medy)
      }
    }
    */
    
    /**
     * Theil-Sen nonparametric estimator of slope and intercept of a line, with confidence bounds.
     * Note that intercept confidence is conditional on the slope being exactly right; you can
     * calculate a new intercept (with bounds) for a new slope `s` with
     * `StdDev((xs,ys).zipped.map( (x,y) => y - s*x ))` or a faster equivalent.
     * Method modified from Fernandes and Leblanc, Remote Sensing of Environment vol 95 pp 303-316 (2005).
     * @param x The ordinates for the fit (x-values)
     * @param y The abscissas for the fit (y-values)
     * @param i0 The index at which to start
     * @param i1 The index at which to end (inclusive)
     * @param maxExplicit The maximum number of points to try to fit explicitly.  Note that this requires time and space proportional to the square.
     * @param storage An array of doubles to store pairwise points; must be at least `1.5*N*(N-1)` long, where `N` is `min(maxExplicit, 1+i1-i0).sq`.  Storage will be allocated if insufficient.
     * @param rng A stream of random numbers to use for non-explicit (sampled) estimates.
     */
    def theilsen(x: Array[Double], y: Array[Double])(
      i0: Int = 0, i1: Int = min(x.length,y.length)-1, maxExplicit: Int = 256, storage: Array[Double] = null, rng: Rng = null
    ): (StandardDeviation, StandardDeviation) = {
      val N = if (i1<i0) 0 else 1+i1-i0
      if (maxExplicit < 32 && N > maxExplicit) throw new IllegalArgumentException("maxExplicit should be at least 32")
      val slope = {
        if (N <= maxExplicit) {
          if ((N.toLong*(N-1)/2)*3 >= Int.MaxValue) throw new IllegalArgumentException("Too large to calculate explicit Theil-Sen pairs.")
          if (N < 2) throw new IllegalArgumentException("Must have at least two points to have a slope.")
          val M = N*(N-1)/2
          val ss = {
            if (storage != null && storage.length >= 3*M) storage
            else new Array[Double](3*M)
          }
          var k = 0
          var i = i0
          while (i <= i1) {
            var j = i+1
            while (j <= i1) {
              if (x(i) != x(j)) {
                val s = (y(j)-y(i))/(x(j)-x(i))
                if (s.finite) {
                  ss(k+2*M) = s
                  k += 1
                }
              }
              j += 1
            }
            i += 1
          }
          System.arraycopy(ss,2*M,ss,0,k)
          val slope = rankFractionD(ss, 0.5, 0, k, k)
          val x2 = java.util.Arrays.copyOfRange(x, i0, i1+1)
          java.util.Arrays.sort(x2)
          var nt = N*(N-1)*(2*N+5).toLong
          i = 1
          while (i < x2.length) {
            if (x2(i)==x2(i-1)) {
              var j = i+1
              while (j < x2.length && x(j) == x(i)) j += 1
              val t = j-i
              nt += t*(t-1)*(2*t+5).toLong
              i = j
            }
            i += 1
          }
          val fdM = 0.05885*sqrt(nt)/k    // Magic factor of 0.05885 determined experimentally; roughly independent of noise and number of points
          System.arraycopy(ss,2*M,ss,0,k)
          val slh = rankFractionD(ss, 0.5+fdM, 0, k, k)
          System.arraycopy(ss,2*M,ss,0,k)
          val sll = rankFractionD(ss, 0.5-fdM, 0, k, k)
          StdDev(N, slope, (slh-sll)*sqrt(N-1))
        }
        else {
          val parts = math.ceil(N/maxExplicit).toInt
          val depth = math.ceil(N/parts).toInt
          val rn = if (rng != null) rng else (new Rng.Hybrid2).seedWithTime
          val dx = new Array[Double](N)
          val dy = new Array[Double](N)
          val picks = new Array[Int](N)
          var i = 0
          while (i < picks.length) {
            picks(i) = i
            i += 1
          }
          Rng.permuteInPlace(picks, rn)
          i = 0
          while (i < N) {
            dx(i) = x(picks(i))
            dy(i) = y(picks(i))
            i += 1
          }
          i = 0
          var j = 0
          val storage = new Array[Double](2*maxExplicit*(maxExplicit-1))
          val slopes = new Array[StandardDeviation](parts)
          while (j < parts) {
            val n = if ((parts-j)*depth != N-i) depth-1 else depth
            slopes(j) = theilsen(dx, dy)(i, i+n-1, maxExplicit, storage, rn)._1
            i += n
            j += 1
          }
          if (parts >= 32) StdDev(slopes.map(_.mean))
          else {
            val sdm = StdDev(slopes.map(_.mean))
            val sds = StdDev(slopes.map(_.sem))
            StdDev(N, sdm.mean, sqrt((N-1)/(parts-1))*sds.mean)
          }
        }
      }
      val intercept = {
        val asd = new AccumulateSD
        var i = i0
        val s = slope.mean
        while (i <= i1) {
          asd +~ (y(i) - s*x(i))
          i += 1
        }
        asd.fixed
      }
      (slope, intercept)
    }
  }
  
  sealed trait SubDistribution[A] { def dists: Seq[(Distribution[A], String)]; def divided: Boolean }
  final case class NoSubDist[A]() extends SubDistribution[A] { def dists = Nil; def divided = false }
  final case class OutlierDist[A](inliers: Distribution[A], outliers: Array[A]) extends SubDistribution[A] { def dists = List((inliers, "* ")); def divided = inliers.sub.divided }
  final case class BimodalDist[A](low: Distribution[A], high: Distribution[A]) extends SubDistribution[A] { def dists = List((low,"v "), (high, "^ ")); def divided = true }
  final case class TimeVaryingDist[A](early: Distribution[A], breakpoint: Int, late: Distribution[A]) extends SubDistribution[A] { 
    def dists = List((early, "< "), (late, "> "))
    def divided = true
  }
  case class Distribution[A](
    n: Int, mean: Double, sd: Double,
    min: Double, med: Double, max: Double, fwhm: Double,
    sub: SubDistribution[A] = NoSubDist[A](),
    data: Distribution.Secretly[Array[A]] = new Distribution.NoSecret[Array[A]]
  ) extends StandardDeviation {
    val robustN = sub match { case OutlierDist(d, _) => d.n; case _ => n }
    def plausiblyRobust = robustN > 10 && !sub.divided
    val robustSD = fwhm2sigma(fwhm)*math.sqrt(robustN/(math.max(1,robustN-1)))
    def robustSEM = fwhm2sigma(fwhm)/math.max(robustN-1,1)
    def sigma = fwhm2sigma(fwhm)
    def robustP(x: Double) = cdfNormal((x - med)/robustSD)
    def robustX(p: Double) = icdfNormal(p)*robustSD + med
    @tailrec final def dropOutliers: Distribution[A] = sub match {
      case OutlierDist(in,_) => in.dropOutliers
      case _ => this
    }
    override lazy val toString: String = { 
      val s = super.toString
      val ss = s +: sub.dists.flatMap{ case (d,t) => d.toString.split('\n').map(t + _) }
      ss.mkString("\n ")
    }
  }
  object Distribution {
    trait Secretly[A] {
      def value: A
      def isDefined: Boolean
      override def toString = "..."
      override def hashCode = 0
      override def equals(a: Any) = a.asInstanceOf[AnyRef].getClass == value.getClass
    }
    class Secret[A](val value: A) extends Secretly[A] { def isDefined = true }
    class NoSecret[A] extends Secretly[A] { def isDefined = false; def value = throw new NoSuchElementException("No secret data.") }
    val zeroOutliers = Array[Int]()
    def zero[A: ClassTag] = Distribution[A](0, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN)
    // Assumes that data is coming in in random order (i.e. unsorted); looks for jumps in mean value
    def findBreakpoint[@specialized(Float, Double) A](a: Array[A], significance: Double = 0.05, intrinsicError: Double = 0.0)(implicit f: Fn1D[A]): Option[Int] = if (a.length < 6) None else {
      val as0, as1 = new AccumulateSD
      var i = 0
      while (i < a.length) {
        val x = f(a(i))
        if (isFinite(x)) as0 +~ x
        i += 1
      }
      if (as0.n < 6) None else {
        val single = as0.fixed
        var split = -1
        var pval = Double.PositiveInfinity
        var rss = Double.PositiveInfinity
        var delta = 0.0
        i = a.length
        val half = a.length/2
        def update(x: Double, down: Boolean) = {
          if (isFinite(x)) {
            if (down) { as0 -~ x; as1 +~ x } else { as0 +~ x; as1 -~ x }
            // Compare residuals for two Gaussians to see if it's better than one; also check t-test for small n (more sensitive, but Bonferroni is too pessimistic)
            val r = as0.sumOfSquares + as1.sumOfSquares
            val prt = Parametric.regressionTest(1, single.sumOfSquares, 3, r, single.n)
            val ptt = if (min(as0.n,as1.n) < 6 || single.n < 20) as0.tTest(as1)*(single.n - 2) else prt
            val p = min(prt, ptt)
            if ((pval > EpsDouble100x && p < pval) || (pval<=EpsDouble100x && p<=EpsDouble100x && r < rss)) { pval = p; rss = r; delta = as0.mean-as1.mean; split = i }
          }
        }
        // Go down from the top
        while (i > half) {
          i -= 1
          update(f(a(i)), true)
        }
        // Now go up from the bottom to avoid major subtraction errors with tiny numbers of elements
        as0.reset
        as1.reset ++~ single
        i = 0
        while (i < half) {
          update(f(a(i)), false)
          i += 1
        }
        if (split > 0 && pval < significance && abs(delta) >= abs(intrinsicError*icdfNormal(pval*0.5))) Some(split) else None
      }
    }
    // Array must be sorted and finite over range of indices
    def probableBimodality[@specialized(Float, Double) A: ClassTag](a: Array[A], start: Int = 0, end: Int = -1, intrinsicError: Double = 0.0)(implicit f: Fn1D[A]): (Double, Int) = {
      val ia = if (start < 0) a.length+start else start
      val ib = if (end < 0) a.length+end+1 else end
      if (ib-ia < 6) (Double.NaN, -1) else {
        val b = {
          val xs = new Array[Double](ib-ia)
          xs(0) = f(a(ia))
          var i = ia+1
          var j = 0
          while (i < ib) { val y = f(a(i)); if (!(xs(j) <= y)) throw new IllegalArgumentException("Array not sorted") else { j += 1; xs(j) = y; i += 1 } }
          xs
        }
        val N = b.length
        val single = StdDev(b)
        val as0 = (new AccumulateSD)
        val as1 = (new AccumulateSD) ++~ single
        @inline def sw(i: Int): Int = {
          val x0 = b(i)
          as0 +~ x0
          as1 -~ x0
          var j = i+1
          while (j < N) {
            val x = b(j)
            if (x != x0) return j
            as0 +~ x
            as1 -~ x
            j += 1
          }
          j
        }
        var i = sw(0)
        if (i < N) i = sw(i)
        if (as1.n < 2 || as1.sumOfSquares < EpsDouble100x) return (Double.NaN, -1)
        var il = 0; { val x0 = as0.mean; while (b(il)<x0) il += 1 }
        var ir = N-1; { val x1 = as1.mean; while (b(ir)>x1) ir -= 1 }; ir += 1
        var xa = as0.mean
        var xb = as1.mean
        var px = single.p( 0.5*(b(i) + b(i-1)) )
        var xl = single.x(0.5*(px + single.p(xa)))
        var xr = single.x(0.5*(px + single.p(xb)))
        var sl, ssl, sscl, sr, ssr, sscr = 0.0
        var k = il
        while (k < i) {
          val x = b(k)
          sl += x
          ssl += { val y = (x-xa); y*y }
          sscl += { val y = (x-xl); y*y }
          k += 1
        }
        while (k < ir) {
          val x = b(k)
          sr += x
          ssr += { val y = (x-xb); y*y }
          sscr += { val y = (x-xr); y*y }
          k += 1
        }
        var mrat = (sscr+sscl)/(ssr+ssl)
        var irat = i
        var nrat = ir-il
        var i_ = i
        while (as1.n > 2 && { i_ = i; i = sw(i); as1.n > 1 }) {
          @inline def rssshift(s: Double, m0: Double, m1: Double, n: Int) = { val m = m0 - m1; 2*(s - n*m0)*m + n*m*m }
          val xa_ = xa
          val xl_ = xl
          val xr_ = xr
          val xb_ = xb
          xa = as0.mean
          xb = as1.mean
          px = single.p( 0.5*(b(i)+b(i-1)) )
          xl = single.x(0.5*(px + single.p(xa)))
          xr = single.x(0.5*(px + single.p(xb)))
          ssl += rssshift(sl, xa_, xa, i_ - il)
          sscl += rssshift(sl, xl_, xl, i_ - il)
          sscr += rssshift(sr, xr_, xr, ir - i_)
          ssr += rssshift(sr, xb_, xb, ir - i_)
          var x = b(il)
          while (x < xa) {
            sl -= x
            ssl -= { val y = (x - xa); y*y }
            sscl -= { val y = (x - xl); y*y }
            il += 1
            x = b(il)
          }
          k = i_
          while (k < i) {
            val x = b(k)
            sl += x
            ssl += { val y = (x - xa); y*y }
            sscl += { val y = (x - xl); y*y }
            sscr -= { val y = (x - xr); y*y }
            ssr -= { val y = (x - xb); y*y }
            sr -= x
            k += 1
          }
          x = b(ir)
          while (x < xb) {
            sr += x
            ssr += { val y = (x - xb); y*y }
            sscr += { val y = (x - xr); y*y }
            ir += 1
            if (ir < b.length) x = b(ir) else x = xb   // TODO--find out why this conditional is necessary
          }
          if (ir < b.length-1) {  // TODO--why is if-statement this necessary?
            val rat = (sscr+sscl)/(ssr+ssl+nrat*intrinsicError*intrinsicError)
            if (rat > mrat) { mrat = rat; irat = i; nrat = ir-il }
          }
        }
        // Fake residual sum of squares since all that matters is the ratio; halve dfs because we only look at half the data
        val p = if (mrat <= 1) 1.0 else Parametric.regressionTest(1, mrat, 2, 1, max(5,nrat))   
        (p, irat)
      }
    }
    def compute[@specialized(Float, Double) A: ClassTag](
      a: Array[A], checkOutliers: Boolean = true, checkCluster: Boolean = true, checkTemporal: Boolean = true, intrinsicError: Double = 0.0, maxNesting: Int = 20
    )(
      implicit f: Fn1D[A]
    ): Distribution[A] = {
      if (a.length == 0) return zero[A]

      val as = new AccumulateSD
      var i = 0
      while (i < a.length) {
        val x = f(a(i))
        if (isFinite(x)) as +~ x
        i += 1
      }
      if (as.n == 0) return zero[A].copy(sub = OutlierDist(zero[A], a))
      
      val finite = {
        if (as.n == a.length) a
        else {
          val b = new Array[A](as.n)
          i = 0
          var j = 0
          while (j < b.length) { val x = f(a(i)); if (isFinite(x)) { b(j) = a(i); j += 1 }; i += 1 }
          b
        }
      }
      val sorted = new Array[A](finite.length)
      System.arraycopy(finite, 0, sorted, 0 , sorted.length)
      sorted match {
        case ad: Array[Double] if (f eq identityDoubleFn) => java.util.Arrays.sort(ad)
        case af: Array[Float] if (f eq floatToDoubleFn) => java.util.Arrays.sort(af)
        case _ => scala.util.Sorting.stableSort(sorted, (a: A, b: A) => f(a) < f(b))
      }
      val dist = Distribution[A](
        as.n,
        as.mean,
        as.sd,
        f(sorted(0)),
        if (sorted.length==1) f(sorted(0)) else if ((sorted.length&0x1) == 1) f(sorted(sorted.length/2)) else 0.5*(f(sorted(sorted.length/2))+f(sorted(sorted.length/2-1))),
        f(sorted(sorted.length-1)),
        if (sorted.length==1) 0.0 else if (sorted.length==2) f(sorted(1))-f(sorted(0)) else {
          val ix = sorted.length*0.25-0.5
          val i = round(ix).toInt
          val xi = if ((ix-i) < EpsDouble100x) f(sorted(i)) else {
            val ii = if (ix > i) i + 1 else i - 1
            val q = abs(ix-i)
            f(sorted(ii))*q + f(sorted(i))*(1-q)
          }
          val jx = sorted.length*0.75 - 0.5
          val j = round(jx).toInt
          val xj = if ((jx-j) < EpsDouble100x) f(sorted(j)) else {
            val jj = if (jx > j) j + 1 else j - 1
            val q = abs(jx-j)
            f(sorted(jj))*q + f(sorted(j))*(1-q)
          }
          xj - xi
        },
        data = new Secret(finite)
      )
      if (dist.sd==0 || dist.n < 6) return dist
      
      // Check for outliers
      if (dist.sd != 0 && maxNesting > 0) {
        var oi0 = -1
        var i0 = 0
        var i1 = finite.length-1
        var oi1 = finite.length
        var xl = f(sorted(0))
        var xr = f(sorted(sorted.length-1))
        if (xl >= xr) return dist     // Everyone is the same, no variation
        while ((i1-i0) > 2 && (oi1 > i1 || oi0 < i0)) {
          val N = 1+i1-i0
          oi0 = i0
          oi1 = i1
          val zl = N*0.1 - 0.5
          val jl = max(0,round(zl).toInt)+i0
          val zr = N*0.9 - 0.5
          val jr = min(N-1,round(zr).toInt)+i0
          val yl = (1 - abs(zl-jl))*f(sorted(jl)) + abs(zl-jl)*f(sorted(jl + (if (zl>jl) 1 else if (jl>0) -1 else 0)))
          val yr = (1 - abs(zr-jr))*f(sorted(jr)) + abs(zr-jr)*f(sorted(jr + (if (zl<jl) -1 else if (jr<sorted.length-1) 1 else 0)))
          if (abs(yr-yl)/max(yr,yl) > EpsDouble100x) {
            val sig = (yr-yl)*OverNinetyPctSigmas + abs(intrinsicError)     // Might be systematic intrinsic error; don't use rms error
            val ip = -icdfNormal(0.001/dist.n)
            xl = max(xl, min(yl, dist.med - ip*sig))
            xr = min(xr, max(yr, dist.med + ip*sig))
            while (f(sorted(i0)) < xl) i0 += 1
            while (f(sorted(i1)) > xr) i1 -= 1
          }
        }
        if ((i0 > 0 || i1 < finite.length-1) && (i1-i0 > 1)) {
          val a2 = new Array[A](1+i1-i0)
          val out = new Array[A](finite.length-a2.length)
          i = 0
          var j,k = 0
          while (i < finite.length) {
            val x = f(finite(i))
            if (x < xl || x > xr) { out(k) = finite(i); k += 1 }
            else { a2(j) = finite(i); j += 1 }
            i += 1
          }
          return dist.copy(sub = OutlierDist(compute(a2, true, checkCluster, checkTemporal, intrinsicError, maxNesting-1), out))
        }
      }
      
      // Check for temporal variation if requested
      if (checkTemporal && dist.sd != 0 && maxNesting > 0) {
        val oi = findBreakpoint(finite, intrinsicError = intrinsicError)
        oi match {
          case Some(n) if (n > 1 && finite.length-n > 1) =>   // TODO -- why doesn't findBreakpoint take care of this?
            val lo = new Array[A](n); System.arraycopy(finite, 0, lo, 0, n)
            val hi = new Array[A](finite.length-n); System.arraycopy(finite, n, hi, 0, hi.length)
            return dist.copy(sub = TimeVaryingDist(compute(lo, checkOutliers, checkCluster, true, intrinsicError, maxNesting-1), n, compute(hi, checkOutliers, checkCluster, true, intrinsicError, maxNesting-1)))
          case _ =>
        }
      }
      
      // Check for multiple distributions
      if (checkCluster && dist.sd != 0 && maxNesting > 0) {
        val (p,n) = probableBimodality(sorted, intrinsicError = intrinsicError)
        if (p < 0.5 && n > 0) {
          val x = 0.5*(f(sorted(n))+f(sorted(n-1)))
          val xs = new Array[A](n)
          val ys = new Array[A](finite.length-n)
          var j,k = 0
          i = 0
          while (i < finite.length) {
            if ((f(finite(i)) < x || k==ys.length) && j < xs.length) { xs(j) = finite(i); j += 1 }
            else { ys(k) = finite(i); k += 1 }
            i += 1
          }
          return dist.copy(sub = BimodalDist(compute(xs, checkOutliers, true, checkTemporal, intrinsicError, maxNesting-1), compute(ys, checkOutliers, true, checkTemporal, intrinsicError, maxNesting-1)))
        }
      }
      
      dist
    }
  }

  def argmedian5[@specialized(Float, Double) A](a0: Array[A], j: Int)(implicit f: Fn1D[A]): Int = {
    val x0 = f(a0(j))
    val x1 = f(a0(j+1))
    val x2 = f(a0(j+2))
    val x3 = f(a0(j+3))
    val x4 = f(a0(j+4))
    val n01 = if (x1<x0) 1 else 0
    val n02 = if (x2<x0) 1 else 0
    val n03 = if (x3<x0) 1 else 0
    val n04 = if (x4<x0) 1 else 0
    if (n01+n02+n03+n04 == 2) return 0
    val n12 = if (x2<x1) 1 else 0
    val n13 = if (x3<x1) 1 else 0
    val n14 = if (x4<x1) 1 else 0
    if ((1-n01)+n12+n13+n14 == 2) return 1
    val n23 = if (x3<x2) 1 else 0
    val n24 = if (x4<x2) 1 else 0
    if ((2-n02-n12)+n23+n24 == 2) return 2
    val n34 = if (x4<x3) 1 else 0
    if ((3-n03-n13-n23)+n34 == 2) 3 else 4
  }
  def argmedian5F(a0: Array[Float], j: Int): Int = {
    val x0 = a0(j)
    val x1 = a0(j+1)
    val x2 = a0(j+2)
    val x3 = a0(j+3)
    val x4 = a0(j+4)
    val n01 = if (x1<x0) 1 else 0
    val n02 = if (x2<x0) 1 else 0
    val n03 = if (x3<x0) 1 else 0
    val n04 = if (x4<x0) 1 else 0
    if (n01+n02+n03+n04 == 2) return 0
    val n12 = if (x2<x1) 1 else 0
    val n13 = if (x3<x1) 1 else 0
    val n14 = if (x4<x1) 1 else 0
    if ((1-n01)+n12+n13+n14 == 2) return 1
    val n23 = if (x3<x2) 1 else 0
    val n24 = if (x4<x2) 1 else 0
    if ((2-n02-n12)+n23+n24 == 2) return 2
    val n34 = if (x4<x3) 1 else 0
    if ((3-n03-n13-n23)+n34 == 2) 3 else 4
  }
  def argmedian5D(a0: Array[Double], j: Int): Int = {
    val x0 = a0(j)
    val x1 = a0(j+1)
    val x2 = a0(j+2)
    val x3 = a0(j+3)
    val x4 = a0(j+4)
    val n01 = if (x1<x0) 1 else 0
    val n02 = if (x2<x0) 1 else 0
    val n03 = if (x3<x0) 1 else 0
    val n04 = if (x4<x0) 1 else 0
    if (n01+n02+n03+n04 == 2) return 0
    val n12 = if (x2<x1) 1 else 0
    val n13 = if (x3<x1) 1 else 0
    val n14 = if (x4<x1) 1 else 0
    if ((1-n01)+n12+n13+n14 == 2) return 1
    val n23 = if (x3<x2) 1 else 0
    val n24 = if (x4<x2) 1 else 0
    if ((2-n02-n12)+n23+n24 == 2) return 2
    val n34 = if (x4<x3) 1 else 0
    if ((3-n03-n13-n23)+n34 == 2) 3 else 4
  }
  def median5[@specialized(Float, Double) A](a0: Array[A], j: Int)(implicit f: Fn1D[A]): A = {
    val x0 = f(a0(j))
    val x1 = f(a0(j+1))
    val x2 = f(a0(j+2))
    val x3 = f(a0(j+3))
    val x4 = f(a0(j+4))
    val n01 = if (x1<x0) 1 else 0
    val n02 = if (x2<x0) 1 else 0
    val n03 = if (x3<x0) 1 else 0
    val n04 = if (x4<x0) 1 else 0
    if (n01+n02+n03+n04 == 2) return a0(j+0)
    val n12 = if (x2<x1) 1 else 0
    val n13 = if (x3<x1) 1 else 0
    val n14 = if (x4<x1) 1 else 0
    if ((1-n01)+n12+n13+n14 == 2) return a0(j+1)
    val n23 = if (x3<x2) 1 else 0
    val n24 = if (x4<x2) 1 else 0
    if ((2-n02-n12)+n23+n24 == 2) return a0(j+2)
    val n34 = if (x4<x3) 1 else 0
    if ((3-n03-n13-n23)+n34 == 2) a0(j+3) else a0(j+4)
  }
  def median5F(a0: Array[Float], j: Int): Float = {
    val x0 = a0(j)
    val x1 = a0(j+1)
    val x2 = a0(j+2)
    val x3 = a0(j+3)
    val x4 = a0(j+4)
    val n01 = if (x1<x0) 1 else 0
    val n02 = if (x2<x0) 1 else 0
    val n03 = if (x3<x0) 1 else 0
    val n04 = if (x4<x0) 1 else 0
    if (n01+n02+n03+n04 == 2) return x0
    val n12 = if (x2<x1) 1 else 0
    val n13 = if (x3<x1) 1 else 0
    val n14 = if (x4<x1) 1 else 0
    if ((1-n01)+n12+n13+n14 == 2) return x1
    val n23 = if (x3<x2) 1 else 0
    val n24 = if (x4<x2) 1 else 0
    if ((2-n02-n12)+n23+n24 == 2) return x2
    val n34 = if (x4<x3) 1 else 0
    if ((3-n03-n13-n23)+n34 == 2) x3 else x4
  }
  def median5D(a0: Array[Double], j: Int): Double = {
    val x0 = a0(j)
    val x1 = a0(j+1)
    val x2 = a0(j+2)
    val x3 = a0(j+3)
    val x4 = a0(j+4)
    val n01 = if (x1<x0) 1 else 0
    val n02 = if (x2<x0) 1 else 0
    val n03 = if (x3<x0) 1 else 0
    val n04 = if (x4<x0) 1 else 0
    if (n01+n02+n03+n04 == 2) return x0
    val n12 = if (x2<x1) 1 else 0
    val n13 = if (x3<x1) 1 else 0
    val n14 = if (x4<x1) 1 else 0
    if ((1-n01)+n12+n13+n14 == 2) return x1
    val n23 = if (x3<x2) 1 else 0
    val n24 = if (x4<x2) 1 else 0
    if ((2-n02-n12)+n23+n24 == 2) return x2
    val n34 = if (x4<x3) 1 else 0
    if ((3-n03-n13-n23)+n34 == 2) x3 else x4
  }
  
  def nearlyMinF(a: Array[Float], k: Int, start: Int = 0, end: Int = -1): Float = {
    val i0 = if (start >= 0) start else a.length+start
    val i1 = if (end >= 0) end else a.length+end+1
    val ai = new Array[Float](math.max(0,k)+1)
    var i = i0+1
    var j = 0
    ai(0) = a(i0)
    while (i < i1) {
      if (a(i) < ai(j)) {
        var h = j-1
        if (j<k) { ai(j+1) = ai(j); j += 1 }
        while (h>=0 && a(i) < ai(h)) { ai(h+1) = ai(h); h -= 1 }
        ai(h+1) = a(i)
      }
      else if (j<k) {
        j += 1
        ai(j) = a(i)
      }
      i += 1
    }
    ai(k)
  }
  def nearlyMaxF(a: Array[Float], k: Int, start: Int = 0, end: Int = -1): Float = {
    val i0 = if (start >= 0) start else a.length+start
    val i1 = if (end >= 0) end else a.length+end+1
    val ai = new Array[Float](math.max(0,k)+1)
    var i = i0+1
    var j = 0
    ai(0) = a(i0)
    while (i < i1) {
      if (a(i) > ai(j)) {
        var h = j-1
        if (j<k) { ai(j+1) = ai(j); j += 1 }
        while (h>=0 && a(i) > ai(h)) { ai(h+1) = ai(h); h -= 1 }
        ai(h+1) = a(i)
      }
      else if (j<k) {
        j += 1
        ai(j) = a(i)
      }
      i += 1
    }
    ai(k)
  }
  def nearlyMinD(a: Array[Double], k: Int, start: Int = 0, end: Int = -1): Double = {
    val i0 = if (start >= 0) start else a.length+start
    val i1 = if (end >= 0) end else a.length+end+1
    val ai = new Array[Double](math.max(0,k)+1)
    var i = i0+1
    var j = 0
    ai(0) = a(i0)
    while (i < i1) {
      if (a(i) < ai(j)) {
        var h = j-1
        if (j<k) { ai(j+1) = ai(j); j += 1 }
        while (h>=0 && a(i) < ai(h)) { ai(h+1) = ai(h); h -= 1 }
        ai(h+1) = a(i)
      }
      else if (j<k) {
        j += 1
        ai(j) = a(i)
      }
      i += 1
    }
    ai(k)
  }
  def nearlyMaxD(a: Array[Double], k: Int, start: Int = 0, end: Int = -1): Double = {
    val i0 = if (start >= 0) start else a.length+start
    val i1 = if (end >= 0) end else a.length+end+1
    val ai = new Array[Double](math.max(0,k)+1)
    var i = i0+1
    var j = 0
    ai(0) = a(i0)
    while (i < i1) {
      if (a(i) > ai(j)) {
        var h = j-1
        if (j<k) { ai(j+1) = ai(j); j += 1 }
        while (h>=0 && a(i) > ai(h)) { ai(h+1) = ai(h); h -= 1 }
        ai(h+1) = a(i)
      }
      else if (j<k) {
        j += 1
        ai(j) = a(i)
      }
      i += 1
    }
    ai(k)
  }
  def nearlyMin[@specialized(Float, Double) A: ClassTag](a: Array[A], k: Int, start: Int = 0, end: Int = -1)(implicit f: Fn1D[A]): A = {
    val i0 = if (start >= 0) start else a.length+start
    val i1 = if (end >= 0) end else a.length+end+1
    val ai = new Array[A](math.max(0,k)+1)
    var i = i0+1
    var j = 0
    ai(0) = a(i0)
    while (i < i1) {
      if (f(a(i)) < f(ai(j))) {
        var h = j-1
        if (j<k) { ai(j+1) = ai(j); j += 1 }
        while (h>=0 && f(a(i)) < f(ai(h))) { ai(h+1) = ai(h); h -= 1 }
        ai(h+1) = a(i)
      }
      else if (j<k) {
        j += 1
        ai(j) = a(i)
      }
      i += 1
    }
    ai(k)
  }
  def nearlyMax[@specialized(Float, Double) A: ClassTag](a: Array[A], k: Int, start: Int = 0, end: Int = -1)(implicit f: Fn1D[A]): A = {
    val i0 = if (start >= 0) start else a.length+start
    val i1 = if (end >= 0) end else a.length+end+1
    val ai = new Array[A](math.max(0,k)+1)
    var i = i0+1
    var j = 0
    ai(0) = a(i0)
    while (i < i1) {
      if (f(a(i)) > f(ai(j))) {
        var h = j-1
        if (j<k) { ai(j+1) = ai(j); j += 1 }
        while (h>=0 && f(a(i)) > f(ai(h))) { ai(h+1) = ai(h); h -= 1 }
        ai(h+1) = a(i)
      }
      else if (j<k) {
        j += 1
        ai(j) = a(i)
      }
      i += 1
    }
    ai(k)
  }

  def insSortF(a: Array[Float], start: Int = 0, end: Int = -1) {
    val i0 = if (start < 0) a.length+start else start
    val i1 = if (end < 0) a.length+end+1 else end
    var i=i0+1
    while (i<i1) {
      val x = a(i)
      var j = i
      while (j>i0 && x < a(j-1)) { a(j) = a(j-1); j -= 1 }
      if (j<i) a(j) = x
      i += 1
    }
  }
  def insSortD(a: Array[Double], start: Int = 0, end: Int = -1) {
    val i0 = if (start < 0) a.length+start else start
    val i1 = if (end < 0) a.length+end+1 else end
    var i=i0+1
    while (i<i1) {
      val x = a(i)
      var j = i
      while (j>i0 && x < a(j-1)) { a(j) = a(j-1); j -= 1 }
      if (j<i) a(j) = x
      i += 1
    }
  }
  def insSort[@specialized(Float, Double) A](a: Array[A], start: Int = 0, end: Int = -1)(implicit f: Fn1D[A]) {
    val i0 = if (start < 0) a.length+start else start
    val i1 = if (end < 0) a.length+end+1 else end
    var i=i0+1
    while (i<i1) {
      val x = f(a(i)); val y = a(i)
      var j = i
      while (j>i0 && x < f(a(j-1))) { a(j) = a(j-1); j -= 1 }
      if (j<i) a(j) = y
      i += 1
    }
  }

  def rankIndexF(a0: Array[Float], rank: Int, start: Int = 0, end: Int = -1, scratch: Int = -1, lousySplit: Boolean = false): Float = {
    var i0 = if (start >= 0) start else a0.length+start
    var i1 = if (end >= 0) end else a0.length+end+1
    var n = i1 - i0
    var k = if (rank < 0) a0.length+rank else rank
    if (k <= 16 || n-k-1 <= 16) { if (k < n-k) nearlyMinF(a0, k, i0, i1) else nearlyMaxF(a0, n-k-1, i0, i1) }
    else {
      var s0 = scratch
      val a = {
        if (scratch>=0 && ((scratch < start && n <= start - scratch) || (scratch >= end && n <= a0.length-scratch))) a0
        else {
          val x = new Array[Float](n*2)
          var i = i0
          var h = n
          while (i < i1) {
            val y = a0(i)
            if (!java.lang.Double.isNaN(y)) { x(h) = y; h += 1 }
            i += 1
          }
          if (k >= h-n) return Float.NaN
          i0 = n; i1 = h; s0=0; n = i1-i0
          x
        }
      }
      def medianize(): Float = {
        var i = i0
        var j = s0
        while (i+5 < i1) {
          a(j) = median5F(a, i)
          i += 5
          j += 1
        }
        if (i+2 < i1) {
          if (a(i+1) < a(i)) { val temp = a(i+1); a(i+1) = a(i); a(i) = temp }
          if (i+3 < i1 && a(i+3) < a(i+2)) { val temp = a(i+3); a(i+3) = a(i+2); a(i+2) = temp }
          if (a(i+2) < a(i)) { val temp = a(i+2); a(i+2) = a(i); a(i) = temp }
          a(j) = a(i)
          j += 1
        }
        val nj = j-s0
        rankIndexF(a, math.max(nj/4, math.min(nj-nj/4, round(k*nj.toDouble/n).toInt)), s0, j, j)
      }
      def slightlyMedian(): Float = {
        a(s0) = a(i0+n/6)
        a(s0+1) = a(i0+(n/3))
        a(s0+2) = a(i0+(n/2))
        a(s0+3) = a(i0+(n/3)*2)
        a(s0+4) = a(i0+(n/6)*5)
        median5F(a, s0)
      }
      val x = (if (lousySplit) medianize else slightlyMedian)
      var i = i0
      var h = s0
      val h1 = s0+(n-k)
      var j = i0
      val j1 = i0+k+1
      while (j < j1 && a(i) <= x) { i += 1; j += 1 }
      while (i < i1 && h < h1 && j < j1) {
        if (a(i) <= x) { a(j) = a(i); j += 1 }
        else { a(h) = a(i); h += 1 }
        i += 1
      }
      if (j==j1) {
        while (i < i1) {
          if (a(i) <= x) { a(j) = a(i); j += 1 }
          i += 1
        }
        if (j-i0 == k+1) x
        else if (j==i1) {
          var neqx = 0
          var ii = i0
          while (ii < i1) { if (x==a(ii)) { neqx += 1 }; ii += 1 }
          if (n - neqx <= k) x
          else {
            var jj = i0
            ii = i0
            while (ii < i1) { if (!(x==a(ii))) { a(jj) = a(ii); jj += 1 }; ii += 1 }
            rankIndexF(a, k, i0, jj, s0)
          }
        }
        else rankIndexF(a, k, i0, j, s0, n.toDouble/(j-i0) < 1.1)
      }
      else {
        while (i < i1) {
          if (!(a(i) <= x)) { a(h) = a(i); h += 1 }
          i += 1
        }
        rankIndexF(a, k - (n - (h - s0)), s0, h, i0, n.toDouble/(h-s0) < 1.1)
      }
    }
  }
  def rankFractionF(a0: Array[Float], rank: Double, start: Int = 0, end: Int = -1, scratch: Int = -1, lousySplit: Boolean = false): Float = {
    var i0 = if (start >= 0) start else a0.length+start
    var i1 = if (end >= 0) end else a0.length+end+1
    var n = i1 - i0
    var k = if (rank<0 || rank>1) throw new IllegalArgumentException("Floating point ranks must be between 0 and 1") else round(rank*(n-1)).toInt
    if (k <= 16 || n-k-1 <= 16) { if (k < n-k) nearlyMinF(a0, k, i0, i1) else nearlyMaxF(a0, n-k-1, i0, i1) }
    else {
      var s0 = scratch
      val a = {
        if (scratch>=0 && ((scratch < start && n <= start - scratch) || (scratch >= end && n <= a0.length-scratch))) a0
        else {
          val x = new Array[Float](n*2)
          var i = i0
          var h = n
          while (i < i1) {
            val y = a0(i)
            if (!java.lang.Double.isNaN(y)) { x(h) = y; h += 1 }
            i += 1
          }
          if (h-n<n) k = round(rank*(h-n-1)).toInt
          i0 = n; i1 = h; s0=0; n = i1-i0
          x
        }
      }
      def medianize(): Float = {
        var i = i0
        var j = s0
        while (i+5 < i1) {
          a(j) = median5F(a, i)
          i += 5
          j += 1
        }
        if (i+2 < i1) {
          if (a(i+1) < a(i)) { val temp = a(i+1); a(i+1) = a(i); a(i) = temp }
          if (i+3 < i1 && a(i+3) < a(i+2)) { val temp = a(i+3); a(i+3) = a(i+2); a(i+2) = temp }
          if (a(i+2) < a(i)) { val temp = a(i+2); a(i+2) = a(i); a(i) = temp }
          a(j) = a(i)
          j += 1
        }
        val nj = j-s0
        rankIndexF(a, math.max(nj/4, math.min(nj-nj/4, round(k*nj.toDouble/n).toInt)), s0, j, j)
      }
      def slightlyMedian(): Float = {
        a(s0) = a(i0+n/6)
        a(s0+1) = a(i0+(n/3))
        a(s0+2) = a(i0+(n/2))
        a(s0+3) = a(i0+(n/3)*2)
        a(s0+4) = a(i0+(n/6)*5)
        median5F(a, s0)
      }
      val x = (if (lousySplit) medianize else slightlyMedian)
      var i = i0
      var h = s0
      val h1 = s0+(n-k)
      var j = i0
      val j1 = i0+k+1
      while (j < j1 && a(i) <= x) { i += 1; j += 1 }
      while (i < i1 && h < h1 && j < j1) {
        if (a(i) <= x) { a(j) = a(i); j += 1 }
        else { a(h) = a(i); h += 1 }
        i += 1
      }
      if (j==j1) {
        while (i < i1) {
          if (a(i) <= x) { a(j) = a(i); j += 1 }
          i += 1
        }
        if (j-i0 == k+1) x
        else if (j==i1) {
          var neqx = 0
          var ii = i0
          while (ii < i1) { if (x==a(ii)) { neqx += 1 }; ii += 1 }
          if (n - neqx <= k) x
          else {
            var jj = i0
            ii = i0
            while (ii < i1) { if (!(x==a(ii))) { a(jj) = a(ii); jj += 1 }; ii += 1 }
            rankIndexF(a, k, i0, jj, s0)
          }
        }
        else rankIndexF(a, k, i0, j, s0, n.toDouble/(j-i0) < 1.1)
      }
      else {
        while (i < i1) {
          if (!(a(i) <= x)) { a(h) = a(i); h += 1 }
          i += 1
        }
        rankIndexF(a, k - (n - (h - s0)), s0, h, i0, n.toDouble/(h-s0) < 1.1)
      }
    }
  }
  def rankIndexD(a0: Array[Double], rank: Int, start: Int = 0, end: Int = -1, scratch: Int = -1, lousySplit: Boolean = false): Double = {
    var i0 = if (start >= 0) start else a0.length+start
    var i1 = if (end >= 0) end else a0.length+end+1
    var n = i1 - i0
    var k = if (rank < 0) a0.length+rank else rank
    if (k <= 16 || n-k-1 <= 16) { if (k < n-k) nearlyMinD(a0, k, i0, i1) else nearlyMaxD(a0, n-k-1, i0, i1) }
    else {
      var s0 = scratch
      val a = {
        if (scratch>=0 && ((scratch < start && n <= start - scratch) || (scratch >= end && n <= a0.length-scratch))) a0
        else {
          val x = new Array[Double](n*2)
          var i = i0
          var h = n
          while (i < i1) {
            val y = a0(i)
            if (!java.lang.Double.isNaN(y)) { x(h) = y; h += 1 }
            i += 1
          }
          if (k >= h-n) return Double.NaN
          i0 = n; i1 = h; s0=0; n = i1-i0
          x
        }
      }
      def medianize(): Double = {
        var i = i0
        var j = s0
        while (i+5 < i1) {
          a(j) = median5D(a, i)
          i += 5
          j += 1
        }
        if (i+2 < i1) {
          if (a(i+1) < a(i)) { val temp = a(i+1); a(i+1) = a(i); a(i) = temp }
          if (i+3 < i1 && a(i+3) < a(i+2)) { val temp = a(i+3); a(i+3) = a(i+2); a(i+2) = temp }
          if (a(i+2) < a(i)) { val temp = a(i+2); a(i+2) = a(i); a(i) = temp }
          a(j) = a(i)
          j += 1
        }
        val nj = j-s0
        rankIndexD(a, math.max(nj/4, math.min(nj-nj/4, round(k*nj.toDouble/n).toInt)), s0, j, j)
      }
      def slightlyMedian(): Double = {
        a(s0) = a(i0+n/6)
        a(s0+1) = a(i0+(n/3))
        a(s0+2) = a(i0+(n/2))
        a(s0+3) = a(i0+(n/3)*2)
        a(s0+4) = a(i0+(n/6)*5)
        median5D(a, s0)
      }
      val x = (if (lousySplit) medianize else slightlyMedian)
      var i = i0
      var h = s0
      val h1 = s0+(n-k)
      var j = i0
      val j1 = i0+k+1
      while (j < j1 && a(i) <= x) { i += 1; j += 1 }
      while (i < i1 && h < h1 && j < j1) {
        if (a(i) <= x) { a(j) = a(i); j += 1 }
        else { a(h) = a(i); h += 1 }
        i += 1
      }
      if (j==j1) {
        while (i < i1) {
          if (a(i) <= x) { a(j) = a(i); j += 1 }
          i += 1
        }
        if (j-i0 == k+1) x
        else if (j==i1) {
          var neqx = 0
          var ii = i0
          while (ii < i1) { if (x==a(ii)) { neqx += 1 }; ii += 1 }
          if (n - neqx <= k) x
          else {
            var jj = i0
            ii = i0
            while (ii < i1) { if (!(x==a(ii))) { a(jj) = a(ii); jj += 1 }; ii += 1 }
            rankIndexD(a, k, i0, jj, s0)
          }
        }
        else rankIndexD(a, k, i0, j, s0, n.toDouble/(j-i0) < 1.1)
      }
      else {
        while (i < i1) {
          if (!(a(i) <= x)) { a(h) = a(i); h += 1 }
          i += 1
        }
        rankIndexD(a, k - (n - (h - s0)), s0, h, i0, n.toDouble/(h-s0) < 1.1)
      }
    }
  }
  def rankFractionD(a0: Array[Double], rank: Double, start: Int = 0, end: Int = -1, scratch: Int = -1, lousySplit: Boolean = false): Double = {
    var i0 = if (start >= 0) start else a0.length+start
    var i1 = if (end >= 0) end else a0.length+end+1
    var n = i1 - i0
    var k = if (rank<0 || rank>1) throw new IllegalArgumentException("Floating point ranks must be between 0 and 1 not "+rank) else round(rank*(n-1)).toInt
    if (k <= 16 || n-k-1 <= 16) { if (k < n-k) nearlyMinD(a0, k, i0, i1) else nearlyMaxD(a0, n-k-1, i0, i1) }
    else {
      var s0 = scratch
      val a = {
        if (scratch>=0 && ((scratch < start && n <= start - scratch) || (scratch >= end && n <= a0.length-scratch))) a0
        else {
          val x = new Array[Double](n*2)
          var i = i0
          var h = n
          while (i < i1) {
            val y = a0(i)
            if (!java.lang.Double.isNaN(y)) { x(h) = y; h += 1 }
            i += 1
          }
          if (h-n<n) k = round(rank*(h-n-1)).toInt
          i0 = n; i1 = h; s0=0; n = i1-i0
          x
        }
      }
      def medianize(): Double = {
        var i = i0
        var j = s0
        while (i+5 < i1) {
          a(j) = median5D(a, i)
          i += 5
          j += 1
        }
        if (i+2 < i1) {
          if (a(i+1) < a(i)) { val temp = a(i+1); a(i+1) = a(i); a(i) = temp }
          if (i+3 < i1 && a(i+3) < a(i+2)) { val temp = a(i+3); a(i+3) = a(i+2); a(i+2) = temp }
          if (a(i+2) < a(i)) { val temp = a(i+2); a(i+2) = a(i); a(i) = temp }
          a(j) = a(i)
          j += 1
        }
        val nj = j-s0
        rankIndexD(a, math.max(nj/4, math.min(nj-nj/4, round(k*nj.toDouble/n).toInt)), s0, j, j)
      }
      def slightlyMedian(): Double = {
        a(s0) = a(i0+n/6)
        a(s0+1) = a(i0+(n/3))
        a(s0+2) = a(i0+(n/2))
        a(s0+3) = a(i0+(n/3)*2)
        a(s0+4) = a(i0+(n/6)*5)
        median5D(a, s0)
      }
      val x = (if (lousySplit) medianize else slightlyMedian)
      var i = i0
      var h = s0
      val h1 = s0+(n-k)
      var j = i0
      val j1 = i0+k+1
      while (j < j1 && a(i) <= x) { i += 1; j += 1 }
      while (i < i1 && h < h1 && j < j1) {
        if (a(i) <= x) { a(j) = a(i); j += 1 }
        else { a(h) = a(i); h += 1 }
        i += 1
      }
      if (j==j1) {
        while (i < i1) {
          if (a(i) <= x) { a(j) = a(i); j += 1 }
          i += 1
        }
        if (j-i0 == k+1) x
        else if (j==i1) {
          var neqx = 0
          var ii = i0
          while (ii < i1) { if (x==a(ii)) { neqx += 1 }; ii += 1 }
          if (n - neqx <= k) x
          else {
            var jj = i0
            ii = i0
            while (ii < i1) { if (!(x==a(ii))) { a(jj) = a(ii); jj += 1 }; ii += 1 }
            rankIndexD(a, k, i0, jj, s0)
          }
        }
        else rankIndexD(a, k, i0, j, s0, n.toDouble/(j-i0) < 1.1)
      }
      else {
        while (i < i1) {
          if (!(a(i) <= x)) { a(h) = a(i); h += 1 }
          i += 1
        }
        rankIndexD(a, k - (n - (h - s0)), s0, h, i0, n.toDouble/(h-s0) < 1.1)
      }
    }
  }
  def rankIndex[@specialized (Float, Double) A: ClassTag](a0: Array[A], rank: Int, start: Int = 0, end: Int = -1, scratch: Int = -1, lousySplit: Boolean = false)(implicit f: Fn1D[A]): A = {
    var i0 = if (start >= 0) start else a0.length+start
    var i1 = if (end >= 0) end else a0.length+end+1
    var n = i1 - i0
    var k = if (rank < 0) a0.length+rank else rank
    if (k <= 16 || n-k-1 <= 16) { if (k < n-k) nearlyMin(a0, k, i0, i1) else nearlyMax(a0, n-k-1, i0, i1) }
    else {
      var s0 = scratch
      val a = {
        if (scratch>=0 && ((scratch < start && n <= start - scratch) || (scratch >= end && n <= a0.length-scratch))) a0
        else {
          val x = new Array[A](n*2)
          var i = i0
          var h = n
          while (i < i1) {
            val y = a0(i)
            if (!java.lang.Double.isNaN(f(y))) { x(h) = y; h += 1 }
            i += 1
          }
          if (k >= h-n) throw new IllegalArgumentException("NaN-equivalent values not supported")
          i0 = n; i1 = h; s0=0; n = i1-i0
          x
        }
      }
      def medianize(): A = {
        var i = i0
        var j = s0
        while (i+5 < i1) {
          a(j) = median5(a, i)
          i += 5
          j += 1
        }
        if (i+2 < i1) {
          if (f(a(i+1)) < f(a(i))) { val temp = a(i+1); a(i+1) = a(i); a(i) = temp }
          if (i+3 < i1 && f(a(i+3)) < f(a(i+2))) { val temp = a(i+3); a(i+3) = a(i+2); a(i+2) = temp }
          if (f(a(i+2)) < f(a(i))) { val temp = a(i+2); a(i+2) = a(i); a(i) = temp }
          a(j) = a(i)
          j += 1
        }
        val nj = j-s0
        rankIndex(a, math.max(nj/4, math.min(nj-nj/4, round(k*nj.toDouble/n).toInt)), s0, j, j)
      }
      def slightlyMedian(): A = {
        a(s0) = a(i0+n/6)
        a(s0+1) = a(i0+(n/3))
        a(s0+2) = a(i0+(n/2))
        a(s0+3) = a(i0+(n/3)*2)
        a(s0+4) = a(i0+(n/6)*5)
        median5(a, s0)
      }
      val x = (if (lousySplit) medianize else slightlyMedian); val w = f(x)
      var i = i0
      var h = s0
      val h1 = s0+(n-k)
      var j = i0
      val j1 = i0+k+1
      while (j < j1 && f(a(i)) <= w) { i += 1; j += 1 }
      while (i < i1 && h < h1 && j < j1) {
        if (f(a(i)) <= w) { a(j) = a(i); j += 1 }
        else { a(h) = a(i); h += 1 }
        i += 1
      }
      if (j==j1) {
        while (i < i1) {
          if (f(a(i)) <= w) { a(j) = a(i); j += 1 }
          i += 1
        }
        if (j-i0 == k+1) x
        else if (j==i1) {
          var neqx = 0
          var ii = i0
          while (ii < i1) { if (x==f(a(ii))) { neqx += 1 }; ii += 1 }
          if (n - neqx <= k) x
          else {
            var jj = i0
            ii = i0
            while (ii < i1) { if (!(x==f(a(ii)))) { a(jj) = a(ii); jj += 1 }; ii += 1 }
            rankIndex(a, k, i0, jj, s0)
          }
        }
        else rankIndex(a, k, i0, j, s0, n.toDouble/(j-i0) < 1.1)
      }
      else {
        while (i < i1) {
          if (!(f(a(i)) <= w)) { a(h) = a(i); h += 1 }
          i += 1
        }
        rankIndex(a, k - (n - (h - s0)), s0, h, i0, n.toDouble/(h-s0) < 1.1)
      }
    }
  }
  def rankFraction[@specialized (Float, Double) A: ClassTag](a0: Array[A], rank: Double, start: Int = 0, end: Int = -1, scratch: Int = -1, lousySplit: Boolean = false)(implicit f: Fn1D[A]): A = {
    var i0 = if (start >= 0) start else a0.length+start
    var i1 = if (end >= 0) end else a0.length+end+1
    var n = i1 - i0
    var k = if (rank<0 || rank>1) throw new IllegalArgumentException("Floating point ranks must be between 0 and 1") else round(rank*(n-1)).toInt
    if (k <= 16 || n-k-1 <= 16) { if (k < n-k) nearlyMin(a0, k, i0, i1) else nearlyMax(a0, n-k-1, i0, i1) }
    else {
      var s0 = scratch
      val a = {
        if (scratch>=0 && ((scratch < start && n <= start - scratch) || (scratch >= end && n <= a0.length-scratch))) a0
        else {
          val x = new Array[A](n*2)
          var i = i0
          var h = n
          while (i < i1) {
            val y = a0(i)
            if (!java.lang.Double.isNaN(f(y))) { x(h) = y; h += 1 }
            i += 1
          }
          if (h-n<n) k = round(rank*(h-n-1)).toInt
          i0 = n; i1 = h; s0=0; n = i1-i0
          x
        }
      }
      def medianize(): A = {
        var i = i0
        var j = s0
        while (i+5 < i1) {
          a(j) = median5(a, i)
          i += 5
          j += 1
        }
        if (i+2 < i1) {
          if (f(a(i+1)) < f(a(i))) { val temp = a(i+1); a(i+1) = a(i); a(i) = temp }
          if (i+3 < i1 && f(a(i+3)) < f(a(i+2))) { val temp = a(i+3); a(i+3) = a(i+2); a(i+2) = temp }
          if (f(a(i+2)) < f(a(i))) { val temp = a(i+2); a(i+2) = a(i); a(i) = temp }
          a(j) = a(i)
          j += 1
        }
        val nj = j-s0
        rankIndex(a, math.max(nj/4, math.min(nj-nj/4, round(k*nj.toDouble/n).toInt)), s0, j, j)
      }
      def slightlyMedian(): A = {
        a(s0) = a(i0+n/6)
        a(s0+1) = a(i0+(n/3))
        a(s0+2) = a(i0+(n/2))
        a(s0+3) = a(i0+(n/3)*2)
        a(s0+4) = a(i0+(n/6)*5)
        median5(a, s0)
      }
      val x = (if (lousySplit) medianize else slightlyMedian); val w = f(x)
      var i = i0
      var h = s0
      val h1 = s0+(n-k)
      var j = i0
      val j1 = i0+k+1
      while (j < j1 && f(a(i)) <= w) { i += 1; j += 1 }
      while (i < i1 && h < h1 && j < j1) {
        if (f(a(i)) <= w) { a(j) = a(i); j += 1 }
        else { a(h) = a(i); h += 1 }
        i += 1
      }
      if (j==j1) {
        while (i < i1) {
          if (f(a(i)) <= w) { a(j) = a(i); j += 1 }
          i += 1
        }
        if (j-i0 == k+1) x
        else if (j==i1) {
          var neqx = 0
          var ii = i0
          while (ii < i1) { if (x==f(a(ii))) { neqx += 1 }; ii += 1 }
          if (n - neqx <= k) x
          else {
            var jj = i0
            ii = i0
            while (ii < i1) { if (!(x==f(a(ii)))) { a(jj) = a(ii); jj += 1 }; ii += 1 }
            rankIndex(a, k, i0, jj, s0)
          }
        }
        else rankIndex(a, k, i0, j, s0, n.toDouble/(j-i0) < 1.1)
      }
      else {
        while (i < i1) {
          if (!(f(a(i)) <= w)) { a(h) = a(i); h += 1 }
          i += 1
        }
        rankIndex(a, k - (n - (h - s0)), s0, h, i0, n.toDouble/(h-s0) < 1.1)
      }
    }
  }

  

  trait SortedOnline {
    def low: Float
    def mid: Float
    def high: Float
    def fullsort: this.type
    def nu(f: Float): this.type
  }

  class SortedThree(private var a: Float = Float.NaN, private var b: Float = Float.NaN, private var c: Float = Float.NaN) extends SortedOnline {
    private var o = 0
    private var n = (if (isNaN(a)) 1 else 0) + (if (isNaN(b)) 1 else 0) + (if (isNaN(c)) 1 else 0)
    private var h = 0
    private var i = 1
    private var j = 2
    private def sab { val x=a; a=b; b=x; val q=h; h=i; i=q }
    private def sbc { val x=b; b=c; c=x; val q=i; i=j; j=q }
    private def hab = a >= b
    private def hbc = b >= c
    def fullsort: this.type = {
      if (n==0) {
        if (hab) sab; if (hbc) sbc
        if (hab) sab
      }
      this
    }
    fullsort
    private def asort { if (hab) { sab; if (hbc) { sbc } } }
    private def bsort { if (hbc) { sbc } else if (hab) sab }
    private def csort { if (hbc) { sbc; if (hab) { sab } } }
    def nu(f: Float): this.type = {
      if (n==0 && !isNaN(f)) {
        if (h == o) { h=o+3; a=f; asort }
        else if (j==o) { j=o+3; c=f; csort }
        else { i=o+3; b=f; bsort }
      }
      else {
        n += (if (isNaN(f)) 1 else 0)
        if (h==o) { h=o+3; n -= (if (isNaN(a)) 1 else 0); a=f }
        else if (j==o) { j=o+3; n -= (if (isNaN(c)) 1 else 0); c=f }
        else { i=o+3; n -= (if (isNaN(b)) 1 else 0); b=f }
        if (n==0) fullsort
      }
      o += 1
      this
    }
    def low = if (n>0) Float.NaN else a
    def mid = if (n>0) Float.NaN else b
    def high = if (n>0) Float.NaN else c
    override def toString = s"$a/$h $b/$i $c/$j"
  }

  class SortedFive(private var a: Float = Float.NaN, private var b: Float = Float.NaN, private var c: Float = Float.NaN, private var d: Float = Float.NaN, private var e: Float = Float.NaN) extends SortedOnline {
    private var o = 0
    private var n = (if (isNaN(a)) 1 else 0) + (if (isNaN(b)) 1 else 0) + (if (isNaN(c)) 1 else 0) + (if (isNaN(d)) 1 else 0) + (if (isNaN(e)) 1 else 0)
    private var h = 0
    private var i = 1
    private var j = 2
    private var k = 3
    private var l = 4
    private def sab { val x=a; a=b; b=x; val q=h; h=i; i=q }
    private def sbc { val x=b; b=c; c=x; val q=i; i=j; j=q }
    private def scd { val x=c; c=d; d=x; val q=j; j=k; k=q }
    private def sde { val x=d; d=e; e=x; val q=k; k=l; l=q }
    private def hab = a >= b
    private def hbc = b >= c
    private def hcd = c >= d
    private def hde = d >= e
    def fullsort: this.type = {
      if (n==0) {
        if (hab) sab; if (hbc) sbc; if (hcd) scd; if (hde) sde
        if (hab) sab; if (hbc) sbc; if (hcd) scd
        if (hab) sab; if (hbc) sbc
        if (hab) sab
      }
      this
    }
    fullsort
    private def asort { if (hab) { sab; if (hbc) { sbc; if (hcd) { scd; if (hde) sde } } } }
    private def bsort { if (hbc) { sbc; if (hcd) { scd; if (hde) sde } } else if (hab) sab }
    private def csort { if (hcd) { scd; if (hde) sde } else if (hbc) { sbc; if (hab) sab } }
    private def dsort { if (hde) sde else if (hcd) { scd; if (hbc) { sbc; if (hab) sab } } }
    private def esort { if (hde) { sde; if (hcd) { scd; if (hbc) { sbc; if (hab) sab } } } }
    def nu(f: Float): this.type = {
      if (n==0 && !isNaN(f)) {
        if (h == o) { h=o+5; a=f; asort }
        else if (l==o) { l=o+5; e=f; esort }
        else if (i==o) { i=o+5; b=f; bsort }
        else if (k==o) { k=o+5; d=f; dsort }
        else { j=o+5; c=f; csort }
      }
      else {
        n += (if (isNaN(f)) 1 else 0)
        if (h==o) { h=o+5; n -= (if (isNaN(a)) 1 else 0); a=f }
        else if (l==o) { l=o+5; n -= (if (isNaN(e)) 1 else 0); e=f }
        else if (i==o) { i=o+5; n -= (if (isNaN(b)) 1 else 0); b=f }
        else if (k==o) { k=o+5; n -= (if (isNaN(d)) 1 else 0); d=f }
        else { j=o+5; n -= (if (isNaN(c)) 1 else 0); c=f }
        if (n==0) fullsort
      }
      o += 1
      this
    }
    def low = if (n>0) Float.NaN else a
    def two = if (n>0) Float.NaN else b
    def mid = if (n>0) Float.NaN else c
    def four = if (n>0) Float.NaN else d
    def high = if (n>0) Float.NaN else e
    override def toString = s"$a/$h $b/$i $c/$j $d/$k $e/$l"
  }

  def bootstrapWt(xs: Array[Float], ws: Array[Float], nrep: Int = 2000, nsigma: Float = 1f) = {
    val rng = new scala.util.Random
    val xn = new Array[Float](nrep)
    for (i <- 0 until nrep) {
      var x = 0.0
      var w = 0.0
      var j = 0
      while (j < xs.length) {
        val k = rng.nextInt(xs.length)
        x += xs(k)*ws(k)
        w += ws(k)
        j += 1
      }
      xn(i) = (x/math.max(1e-6,w)).toFloat
    }
    java.util.Arrays.sort(xn)
    (xn(math.round(nrep*cdfNormal(-nsigma).toFloat)) - xn(math.round(nrep*0.5f)), xn(math.round(nrep*cdfNormal(nsigma).toFloat)) - xn(math.round(nrep*0.5f)))
  }
}
