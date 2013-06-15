// Inherit.scala Copyright Rex Kerr 2013 / BSD 2-clause

package inherit

import ichi.bench.Thyme

trait A { def f(i: Int): Int }
class A2 extends A { def f(i: Int) = (i+1)*i }
final class A3 extends A { def f(i: Int) = (i+1)*i }
class A4 extends A { final def f(i: Int) = (i+1)*i }
final class A5 extends A { final def f(i: Int) = (i+1)*i }
abstract class B { def f(i: Int): Int }
class B2 extends B { def f(i: Int) = (i+1)*i }
final class B3 extends B { def f(i: Int) = (i+1)*i }
class B4 extends B { final def f(i: Int) = (i+1)*i }
final class B5 extends B { final def f(i: Int) = (i+1)*i }
class C { def f(i: Int) = (i+1)*i }
class C2 extends C { override def f(i: Int) = (i-1)*i }
final class C3 extends C { override def f(i: Int) = (i-1)*i }
class C4 extends C { final override def f(i: Int) = (i-1)*i }
final class C5 extends C { final override def f(i: Int) = (i-1)*i }
sealed trait D { def f(i: Int): Int; def g(i: Int) = this match {
  case _: D2 => (i+1)*i
  case _: D3 => (i-1)*i
} }
final class D2 extends D { def f(i: Int) = (i+1)*i }
final class D3 extends D { def f(i: Int) = (i-1)*i }
sealed trait E { def f(i: Int): Int; def g(i: Int) = this match {
  case _: E2 => (i+1)*i
  case _: E3 => (i+2)*i
  case _: E4 => (i-1)*i
  case _: E5 => (i-2)*i
} }
final class E2 extends E { def f(i: Int) = (i+1)*i }
final class E3 extends E { def f(i: Int) = (i+2)*i }
final class E4 extends E { def f(i: Int) = (i-1)*i }
final class E5 extends E { def f(i: Int) = (i-2)*i }
sealed trait F { def f(i: Int): Int; def g(i: Int) = this match {
  case _: F2 => (i+1)*i
  case _: F3 => (i+2)*i
  case _: F4 => (i+3)*i
  case _: F5 => (i+4)*i
  case _: F6 => (i-1)*i
  case _: F7 => (i-2)*i
  case _: F8 => (i-3)*i
  case _: F9 => (i-4)*i
} }
final class F2 extends F { def f(i: Int) = (i+1)*i }
final class F3 extends F { def f(i: Int) = (i+2)*i }
final class F4 extends F { def f(i: Int) = (i+3)*i }
final class F5 extends F { def f(i: Int) = (i+4)*i }
final class F6 extends F { def f(i: Int) = (i-1)*i }
final class F7 extends F { def f(i: Int) = (i-2)*i }
final class F8 extends F { def f(i: Int) = (i-3)*i }
final class F9 extends F { def f(i: Int) = (i-4)*i }
final class G { final def f(i: Int) = (i+1)*i }
sealed trait H {
  def f(i: Int): Int
  def g(i: Int) = this match {
    case _: H2 => (i+1)*i
    case _: H3 => (i+2)*i
    case _ => (i-1)*i
  }
  def h(i: Int) = this match {
    case _: H3 => (i+2)*i
    case _ => (i-1)*i
  }
}
final class H2 extends H { def f(i: Int) = (i+1)*i }
final class H3 extends H { def f(i: Int) = (i+2)*i }
final class H4 extends H { def f(i: Int) = (i-1)*i }
sealed trait I { def f(i: Int): Int; def g(i: Int) = this match {
  case ip: IP => ip match {
    case _: I2 => (i+1)*i
    case _ => (i+2)*i
  }
  case in: IN => in match {
    case _: I4 => (i-1)*i
    case _ => (i-2)*i
  }
} }
sealed trait IP extends I {}
sealed trait IN extends I {}
final class I2 extends IP { def f(i: Int) = (i+1)*i }
final class I3 extends IP { def f(i: Int) = (i+2)*i }
final class I4 extends IN { def f(i: Int) = (i-1)*i }
final class I5 extends IN { def f(i: Int) = (i-2)*i }



object Inherit {
  final val N = 1000
  @volatile var mysteryIndex = 0
  def base = { var i,s = 0; while (i<N) { s += (i+1)*i; i += 1 }; s }
  def allg(g: Array[G]) = { var i,s = 0; while (i<N) { s += g(i).f(i); i += 1 }; s }
  def justa2(a: Array[A2]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def a2asa(a: Array[A]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def justa3(a: Array[A3]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def a3asa(a: Array[A]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def justa4(a: Array[A4]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def a4asa(a: Array[A]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def justa5(a: Array[A5]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def a5asa(a: Array[A]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def amisc(a: Array[A]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def justb2(a: Array[B2]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def b2asb(a: Array[B]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def justb3(a: Array[B3]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def b3asb(a: Array[B]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def justb4(a: Array[B4]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def b4asb(a: Array[B]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def justb5(a: Array[B5]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def b5asb(a: Array[B]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def bmisc(a: Array[B]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def justc2(a: Array[C2]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def c2asc(a: Array[C]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def justc3(a: Array[C3]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def c3asc(a: Array[C]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def justc4(a: Array[C4]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def c4asc(a: Array[C]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def justc5(a: Array[C5]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def c5asc(a: Array[C]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def cmisc(a: Array[C]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def df(a: Array[D]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def dg(a: Array[D]) = { var i,s = 0; while (i<N) { s += a(i).g(i); i += 1 }; s }
  def e2f(a: Array[E]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def ef(a: Array[E]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def e2g(a: Array[E]) = { var i,s = 0; while (i<N) { s += a(i).g(i); i += 1 }; s }
  def eg(a: Array[E]) = { var i,s = 0; while (i<N) { s += a(i).g(i); i += 1 }; s }
  def f2f(a: Array[F]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def f4f(a: Array[F]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def ff(a: Array[F]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def f2g(a: Array[F]) = { var i,s = 0; while (i<N) { s += a(i).g(i); i += 1 }; s }
  def f4g(a: Array[F]) = { var i,s = 0; while (i<N) { s += a(i).g(i); i += 1 }; s }
  def fg(a: Array[F]) = { var i,s = 0; while (i<N) { s += a(i).g(i); i += 1 }; s }
  def hf(a: Array[H]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def hg(a: Array[H]) = { var i,s = 0; while (i<N) { s += a(i).g(i); i += 1 }; s }
  def hh(a: Array[H]) = { var i,s = 0; while (i<N) { s += a(i).h(i); i += 1 }; s }
  def If(a: Array[I]) = { var i,s = 0; while (i<N) { s += a(i).f(i); i += 1 }; s }
  def Ig(a: Array[I]) = { var i,s = 0; while (i<N) { s += a(i).g(i); i += 1 }; s }
  def run1(th: Thyme) = {
    val gs = Array.fill(N)(new G)
    println(th.pbenchWarm(th.Warm(base), N, "base"))
    println(th.pbenchWarm(th.Warm(allg(gs)), N, "G"))
  }
  def run2(th: Thyme) = {
    val a2s = Array.fill(N)(new A2)
    val a2z = Array.fill(N)(new A2 : A)
    val a3s = Array.fill(N)(new A3)
    val a3z = Array.fill(N)(new A3 : A)
    val a4s = Array.fill(N)(new A4)
    val a4z = Array.fill(N)(new A4 : A)
    val a5s = Array.fill(N)(new A5)
    val a5z = Array.fill(N)(new A5 : A)
    println
    println(th.pbenchWarm(th.Warm(justa2(a2s)), N, "a2"))
    println(th.pbenchWarm(th.Warm(a2asa(a2z)), N, "a2a"))
    println(th.pbenchWarm(th.Warm(justa3(a3s)), N, "a3"))
    println(th.pbenchWarm(th.Warm(a3asa(a3z)), N, "a3a"))
    println(th.pbenchWarm(th.Warm(justa4(a4s)), N, "a4"))
    println(th.pbenchWarm(th.Warm(a4asa(a4z)), N, "a4a"))
    println(th.pbenchWarm(th.Warm(justa5(a5s)), N, "a5"))
    println(th.pbenchWarm(th.Warm(a5asa(a5z)), N, "a5a"))
  }
  def run3(th: Thyme) = {
    val b2s = Array.fill(N)(new B2)
    val b2z = Array.fill(N)(new B2 : B)
    val b3s = Array.fill(N)(new B3)
    val b3z = Array.fill(N)(new B3 : B)
    val b4s = Array.fill(N)(new B4)
    val b4z = Array.fill(N)(new B4 : B)
    val b5s = Array.fill(N)(new B5)
    val b5z = Array.fill(N)(new B5 : B)
    println
    println(th.pbenchWarm(th.Warm(justb2(b2s)), N, "b2"))
    println(th.pbenchWarm(th.Warm(b2asb(b2z)), N, "b2b"))
    println(th.pbenchWarm(th.Warm(justb3(b3s)), N, "b3"))
    println(th.pbenchWarm(th.Warm(b3asb(b3z)), N, "b3b"))
    println(th.pbenchWarm(th.Warm(justb4(b4s)), N, "b4"))
    println(th.pbenchWarm(th.Warm(b4asb(b4z)), N, "b4b"))
    println(th.pbenchWarm(th.Warm(justb5(b5s)), N, "b5"))
    println(th.pbenchWarm(th.Warm(b5asb(b5z)), N, "b5b"))
  }
  def run4(th: Thyme) = {
    val c2s = Array.fill(N)(new C2)
    val c2z = Array.fill(N)(new C2 : C)
    val c3s = Array.fill(N)(new C3)
    val c3z = Array.fill(N)(new C3 : C)
    val c4s = Array.fill(N)(new C4)
    val c4z = Array.fill(N)(new C4 : C)
    val c5s = Array.fill(N)(new C5)
    val c5z = Array.fill(N)(new C5 : C)
    println
    println(th.pbenchWarm(th.Warm(justc2(c2s)), N, "c2"))
    println(th.pbenchWarm(th.Warm(c2asc(c2z)), N, "c2c"))
    println(th.pbenchWarm(th.Warm(justc3(c3s)), N, "c3"))
    println(th.pbenchWarm(th.Warm(c3asc(c3z)), N, "c3c"))
    println(th.pbenchWarm(th.Warm(justc4(c4s)), N, "c4"))
    println(th.pbenchWarm(th.Warm(c4asc(c4z)), N, "c4c"))
    println(th.pbenchWarm(th.Warm(justc5(c5s)), N, "c5"))
    println(th.pbenchWarm(th.Warm(c5asc(c5z)), N, "c5c"))
  }
  def run5(th: Thyme) = {
    val ds = Array.tabulate(N)(i => if ((i&1)==0) new D2 else new D3)
    println
    println(th.pbenchWarm(th.Warm(df(ds)), N, "df"))
    println(th.pbenchWarm(th.Warm(dg(ds)), N, "dg"))
  }
  def run6(th: Thyme) = {
    val e2s = Array.tabulate(N)(i => if ((i&1)==0) new E2 else new E4)
    val es = Array.tabulate(N)(i => (i&3) match { case 0 => new E2; case 1 => new E3; case 2 => new E4; case _ => new E5 })
    println(th.pbenchWarm(th.Warm(e2f(e2s)), N, "e2f"))
    println(th.pbenchWarm(th.Warm(ef(es)), N, "ef"))
    println(th.pbenchWarm(th.Warm(e2g(e2s)), N, "e2g"))
    println(th.pbenchWarm(th.Warm(eg(es)), N, "eg"))
  }
  def run7(th: Thyme) = {
    val f2s = Array.tabulate(N)(i => if ((i&1)==0) new F2 else new F6)
    val f4s = Array.tabulate(N)(i => (i&3) match { case 0 => new F2; case 1 => new F4; case 2 => new F6; case _ => new F8 })
    val fs = Array.tabulate(N)(i => (i&7) match { case 0 => new F2; case 1 => new F3; case 2 => new F4; case 3 => new F5; case 4 => new F6; case 5 => new F7; case 6 => new F8; case _ => new F9 })
    println(th.pbenchWarm(th.Warm(f2f(f2s)), N, "f2f"))
    println(th.pbenchWarm(th.Warm(f4f(f4s)), N, "f4f"))
    println(th.pbenchWarm(th.Warm(ff(fs)), N, "ff"))
    println(th.pbenchWarm(th.Warm(f2g(f2s)), N, "f2g"))
    println(th.pbenchWarm(th.Warm(f4g(f4s)), N, "f4g"))
    println(th.pbenchWarm(th.Warm(fg(fs)), N, "fg"))
  }
  def run8(th: Thyme) = {
    val hs = Array.tabulate(N)(i => (i%3) match { case 0 => new H2; case 1 => new H3; case _ => new H4 })
    val is = Array.tabulate(N)(i => (i&3) match { case 0 => new I2; case 1 => new I3; case 2 => new I4; case _ => new I5 })
    println
    println(th.pbenchWarm(th.Warm(hf(hs)), N, "hf"))
    println(th.pbenchWarm(th.Warm(hg(hs)), N, "hg"))
    println(th.pbenchWarm(th.Warm(hh(hs)), N, "hh"))
    println(th.pbenchWarm(th.Warm(If(is)), N, "if"))
    println(th.pbenchWarm(th.Warm(Ig(is)), N, "ig"))    
  }
  
  def main(args: Array[String]) {
    val th = Thyme.warmed(verbose = print)
    run1(th)
    run2(th)
    run3(th)
    run4(th)
    run5(th)
    run6(th)
    run7(th)
    run8(th)
  }
} 
