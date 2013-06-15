// ControlFlow.scala Copyright Rex Kerr 2013 / BSD 2-clause

import scala.annotation.tailrec
import scala.util.control.ControlThrowable
import ichi.bench.Thyme
import ichi.core.Nope

object ControlFlow {
  final val N = 1000
  @volatile var mysteryIndex = 0
  def blankSum = { var i,s = 0; while (i<N) { s += (i+1)*i; i += 1 }; s }
  @tailrec def blankTr(i: Int = 0, s: Int = 0): Int = if (i<N) blankTr(i+1, s + (i+1)*i) else s
  def anonSum(f: Int => Int) = { var i,s = 0; while (i<N) { s += f(i); i += 1 }; s }
  def forSum = { var s = 0; for (i <- 0 until N) { s += (i+1)*i }; s }
  def itSum = { var s = 0; Iterator.from(0).take(N).foreach{ i => s += (i+1)*i }; s }
  def andSum = { var i,s = 0; while (i<N) { s += (i+1)*(i&2); i +=1  }; s }
  def andEqSum = { var i,s = 0; while (i<N) { s += (i + (if ((i&2)==2) 1 else -1))*i; i += 1 }; s }
  def ifSum = { var i,s = 0; while (i<N) { s += (if ((i&2)==2) (i+1)*i else (i-1)*i); i += 1 }; s }
  def matchSum = { var i,s = 0; while (i<N) { s += ((i&2) match { case 2 => (i+1)*i; case _ => (i-1)*i }); i += 1 }; s }
  def halfSum = { var i=2; var s=0; while (i<N) { s += i*i + (i+1)*i + (i+1)*(i+1) + (i+1+1)*(i+1); i += 4 }; s }
  def unrolledSum = { var i,s = 0; while (i<N) { if ((i&2)==2) s += i*i + (i+1)*i; i += 1 }; s }
  def whileSum = { var i,s = 0; while (i<N) { var j,t = 0; while (j < (i&2)) { t += (i+j)*i; j += 1 }; s += t; i += 1 }; s }
  @tailrec def trf(i: Int, j: Int = 0, s: Int = 0): Int = if (j < (i&2)) trf(i, j+1, s+(i+j)*i) else s
  def trfSum = { var i,s = 0; while (i<N) { s += trf(i); i += 1 }; s }
  def ret(i: Int): Int = { var j,s = 0; while (true) { if (j >= (i&2)) return s; s += (i+j)*i; j += 1 }; s }  // Crazily gets FASTER if you use while (j<4)!
  def retSum = { var i,s = 0; while (i<N) { s += ret(i); i += 1 }; s }
  def four(i: Int): Int = { var s = 0; for (j <- 0 until (i&2)) s += (i+j)*i; s }
  def fourSum = { var i,s = 0; while (i<N) { s += four(i); i += 1 }; s }
  def toss(i: Int) = { var j,s = 0; try { while (true) { if (j >= (i&2)) throw Nope; s += (i+j)*i; j += 1 }; s } catch { case Nope => s } }
  def tossSum = { var i,s = 0; while (i<N) { s += toss(i); i += 1 }; s }
  def tossout(i: Int, j: Int) = { if (j >= (i&2)) throw Nope; (i+j)*i }
  def catchin(i: Int) = { var j,s = 0; try { while (true) { s += tossout(i,j); j += 1 }; s } catch { case Nope => s } }
  def catchSum = { var i,s = 0; while (i<N) { s += catchin(i); i += 1 }; s }
  def newtoss(i: Int) = { var j,s = 0; try { while (true) { if (j >= (i&2)) throw new ControlThrowable {}; s += (i+j)*i; j += 1 }; s } catch { case _: ControlThrowable => s } }
  def newtossSum = { var i,s = 0; while (i<N) { s += newtoss(i); i += 1 }; s }
  def stacktoss(i: Int) = { var j,s = 0; try { while (true) { if (j >= (i&2)) throw new Exception; s += (i+j)*i; j += 1 }; s } catch { case _: Exception => s } }
  def stacktossSum = { var i,s = 0; while (i<N) { s += stacktoss(i); i += 1 }; s }
  def main(args: Array[String]) {
    val th = Thyme.warmed(verbose = print)
    val wbs = th.Warm{blankSum}
    println(th.pbenchWarm(wbs, N, "blank"))
    val wbt = th.Warm{blankTr()}
    println(th.pbenchWarm(wbt, N, "tailrec-each"))
    val w_s = th.Warm(anonSum(i => (i+1)*i))
    println(th.pbenchWarm(w_s, N, "anonfunc"))
    val wfs = th.Warm(forSum)
    println(th.pbenchWarm(wfs, N, "for-whole"))
    val wIs = th.Warm(itSum)
    println(th.pbenchWarm(wIs, N, "iterator"))
    val was = th.Warm{andSum}
    println(th.pbenchWarm(was, N, "and"))
    val wes = th.Warm{andEqSum}
    println(th.pbenchWarm(wes, N, "indic"))
    val wis = th.Warm{ifSum}
    println(th.pbenchWarm(wis, N, "if"))
    val wms = th.Warm{matchSum}
    println(th.pbenchWarm(wms, N, "match"))
    def whs = th.Warm{halfSum}
    println(th.pbenchWarm(whs, N, "half"))
    val wus = th.Warm{unrolledSum}
    println(th.pbenchWarm(wus, N, "unrolled"))
    val wws = th.Warm{whileSum}
    println(th.pbenchWarm(wws, N, "while"))
    val wts = th.Warm{trfSum}
    println(th.pbenchWarm(wts, N, "tailrec"))
    val wrs = th.Warm{retSum}
    println(th.pbenchWarm(wrs, N, "return"))
    val w4s = th.Warm{fourSum}
    println(th.pbenchWarm(w4s, N, "for-individual"))
    val wxs = th.Warm(tossSum)
    println(th.pbenchWarm(wxs, N, "exception"))
    val wys = th.Warm(catchSum)
    println(th.pbenchWarm(wys, N, "cross-method exception"))
    val wzs = th.Warm(newtossSum)
    println(th.pbenchWarm(wzs, N, "new stackless exception"))
    val wss = th.Warm(stacktossSum)
    println(th.pbenchWarm(wss, N, "new stacked exception"))
  }
}
