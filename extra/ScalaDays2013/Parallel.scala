// Parallel.scala Copyright Rex Kerr 2013 / BSD 2-clause

import ichi.bench.Thyme

object Parallel {
  final val N = 1000
  var M = 1000
  @volatile var running = true
  @volatile var mysteryIndex = 1000
  val atomic = new java.util.concurrent.atomic.AtomicInteger(1000)
  class SneakyMutator(val s: Settable) extends Thread {
    var sum = 0
    override def run() = {
      while (running) {
        val k = M ^ 1
        M = k
        mysteryIndex = k
        atomic.set(k)
        sum += s.a + s.b + s.c.get + s.getD
        Thread.sleep(10)
      }
    }
  }
  case class GoI(var go: Boolean = true, var i: Int = 0) {}
  class WillTellYou(init: GoI, e: java.util.concurrent.Exchanger[GoI]) extends Thread {
    var mine = init
    override def run() = {
      while (running) {
        mine.go = mine.i < N
        mine = e.exchange(mine)
      }
    }
  }
  class Settable {
    var a = 0
    @volatile var b = 0
    val c = new java.util.concurrent.atomic.AtomicInteger(0)
    var d = 0
    val lists = List.range(0,500).map(i => List(2*i, 2*i+1))
    val pars = lists.map(_.par)
    def setD(i: Int) = synchronized { d = i }
    def getD = synchronized { d }
    def addD(i: Int) = synchronized { d += i }
    def useA = { a = 0; var i = 0; while (i<N) { a += (i+1)*i; i +=1 }; a }
    def useB = { b = 0; var i = 0; while (i<N) { b += (i+1)*i; i +=1 }; b }
    def useC = { c.set(0); var i = 0; while (i<N) { c.addAndGet((i+1)*i); i += 1 }; c.get }
    def useD = { setD(0); var i = 0; while (i<N) { addD((i+1)*i); i += 1 }; getD }
    def listy = lists.map(_.map(i => (i+1)*i).sum).sum
    def pary = pars.map(_.map(i => (i+1)*i).sum).sum
  }
  def base = { var i,s = 0; while (i<N) { s += (i+1)*i; i += 1 }; s }
  def localset = { var i,s,t = 0; while (i<N) { t = (i+1)*i; s += (i+1)*i; i += 1 }; s }
  def varloop = { var i,s = 0; while (i<M) { s += (i+1)*i; i += 1 }; s }
  def varset(t: Settable) = { var i,s = 0; while (i<N) { t.a = (i+1)*i; s += (i+1)*i; i += 1 }; s }
  def volloop = { var i,s = 0; while (i<mysteryIndex) { s += (i+1)*i; i += 1 }; s }
  def volset(t: Settable) = { var i,s = 0; while (i<N) { t.b += (i+i)*i; s += (i+1)*i; i += 1 }; s }
  def atomicloop = { var i,s = 0; while (i<atomic.get) { s += (i+1)*i; i += 1 }; s }
  def atomicset(t: Settable) = { var i,s = 0; while (i<N) { t.c.set((i+1)*i); s += (i+1)*i; i += 1 }; s }
  def syncedloop = { var i,s = 0; while (i<Parallel.synchronized{M}) { s += (i+1)*i; i += 1 }; s }
  def syncedset(t: Settable) = { var i,s = 0; while (i<N) { t.setD((i+1)*i); s += (i+1)*i; i += 1 }; s }
  def boxedloop = { var i,s = 0; while (Option(i).filter(_<M).isDefined) { s += (i+1)*i; i +=1 }; s }
  def dualexloop(e: java.util.concurrent.Exchanger[GoI]) = { var gi = GoI(false,0); var s = 0; while ({e.exchange(gi); e.exchange(gi); gi.go}) { s += (gi.i+1)*gi.i; gi.i += 1 }; s }
  def futureloop = {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent._
    var i,s = 0
    while (i < Await.result( Future(M+1), duration.Duration.Inf )) { s += (i+1)*i; i += 1 }
    s
  }
  def main(args: Array[String]) {
    val th = Thyme.warmed(accuracyTarget = 0.05, verbose = print)
    val sm = new SneakyMutator(new Settable)
    sm.start
    val e = new java.util.concurrent.Exchanger[GoI]
    val wty = new WillTellYou(GoI(true,0), e)
    wty.start
    
    println
    println( th.pbenchWarm(th.Warm(base), 1000, "base") )
    println( th.pbenchWarm(th.Warm(varloop), 1000, "varloop") )
    println( th.pbenchWarm(th.Warm(volloop), 1000, "volloop") )
    println( th.pbenchWarm(th.Warm(atomicloop), 1000, "atomic") )
    println( th.pbenchWarm(th.Warm(syncedloop), 1000, "synced") )
    println( th.pbenchWarm(th.Warm(localset), 1000, "local") )
    println( th.pbenchWarm(th.Warm(varset(sm.s)), 1000, "varset") )
    println( th.pbenchWarm(th.Warm(volset(sm.s)), 1000, "volset") )
    println( th.pbenchWarm(th.Warm(atomicset(sm.s)), 1000, "atomicset") )
    println( th.pbenchWarm(th.Warm(syncedset(sm.s)), 1000, "syncedset") )
    println( th.pbenchWarm(th.Warm(sm.s.useA), 1000, "useA") )    
    println( th.pbenchWarm(th.Warm(sm.s.useB), 1000, "useB") )    
    println( th.pbenchWarm(th.Warm(sm.s.useC), 1000, "useC") )    
    println( th.pbenchWarm(th.Warm(sm.s.useD), 1000, "useD") )    
    
    
    println
    println( th.pbenchWarm(th.Warm(boxedloop), 1000, "boxed") )
    println( th.pbenchWarm(th.Warm(dualexloop(e)), 2000, "dualex") )
    println( th.pbenchWarm(th.Warm(futureloop), 1000, "future") )
    running = false
    e.exchange(GoI(false,0))
    println( th.pbenchWarm(th.Warm(sm.s.listy), 500, "2 elt") )
    println( th.pbenchWarm(th.Warm(sm.s.pary), 500, "2 par elt") )
  }
}
