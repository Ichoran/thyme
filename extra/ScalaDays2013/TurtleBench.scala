// TurtleBench.scala Copyright Rex Kerr 2013 / BSD 2-clause
// Run this in the REPL inside SBT
// Put Thyme.jar in lib
// This is ONLY intended to be used in conjunction with Seth Tisue's
// Scala Days 2013 talk example (of shapeless' lenses on turtles)

import rillit.Lenser, shapeless.Lens, ichi.bench.Thyme
val th = Thyme.warmed(verbose = print)
case class Point(x: Double, y: Double) {
  def xF(f: Double => Double) = new Point(f(x),y)
}
case class Turtle(here: Point, there: Point) {
  def hereF(f: Point => Point) = new Turtle(f(here), there)
}
val direct = th.Warm{ var t = Turtle(Point(0,0),Point(0,0)); var i=0; while (i<1024) { t = t.copy(here = t.here.copy(x = t.here.x+1)); i += 1 }; t }
val lensed = th.Warm{ var t = Turtle(Point(0,0),Point(0,0)); var l: Lens[Turtle, Double] = Lenser[Turtle].here.x; var i=0; while (i<1024) { t = l.set(t)(l.get(t)+1); i += 1 }; t }
val funced = th.Warm{ var t = Turtle(Point(0,0),Point(0,0)); var i=0; while (i<1024) { t = t.hereF(_.xF(_+1)); i += 1 }; t }
case class Pnt(var x: Double, var y: Double)
case class Turt(here: Pnt, there: Pnt)
val muted = th.Warm{ val t = Turt(Pnt(0,0), Pnt(0,0)); var i=0; while (i<1024) { t.here.x += 1; i += 1 }; Turtle(Point(t.here.x,t.here.y), Point(t.there.x,t.there.y)) }
th.pbenchWarm(muted, 1024, "mutable")
th.pbenchWarm(direct, 1024, "immutable")
th.pbenchWarm(funced, 1024, "immutable with update")
th.pbenchWarm(lensed, 1024, "immutable with lens")


