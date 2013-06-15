// Collect.scala Copyright Rex Kerr 2013 / BSD 2-clause

import ichi.bench.Thyme

object Collect {
  final val N = 1000
  @volatile var mysteryIndex = 16
  def listinits(xs: List[Int]): Int = xs match {
    case x :: Nil => x
    case _ => listinits(xs.init)
  }
  def vectinits(xs: Vector[Int]): Int = xs match {
    case Vector(x) => x
    case _ => vectinits(xs.init)
  }
  def arrayinits(xs: Array[Int]): Int = xs match {
    case Array(x) => x
    case _ => arrayinits(xs.init)
  }
  def main(args: Array[String]) {
    val th = Thyme.warmed(verbose = print)
    import collection.mutable.{ArrayBuffer => ArB}
    import Array.{tabulate => T}
    
    val range = 0 until N
    val list = List.range(0,N)
    val vect = Vector.range(0,N)
    val arbuf = ArB.range(0,N)
    val arr = Array.range(0,N)
    val set = arr.toSet
    val map = arr.map(i => (i,i+1)).toMap
    val slist = list.map(_.toString)
    val svect = vect.map(_.toString)
    val sarb = arbuf.map(_.toString)
    val sarr = arr.map(_.toString)
    val sset = set.map(_.toString)
    val smap = map.map{ case (k,v) => (k.toString, v.toString) }
    
    println("raw")
    println( th.pbenchWarm( th.Warm{ var i,s=0; while (i<N) { s += i*(i+1); i += 1 }; s }, N, "loop" ) )    
    
    println("\nrange")
    println( th.pbenchWarm( th.Warm{ range.map(i => i*(i+1)).sum }, N, "map" ) )
    println( th.pbenchWarm( th.Warm{ range.view.map(i => i*(i+1)).sum }, N, "view map" ) )    
    println( th.pbenchWarm( th.Warm{ ( 0 /: range)((c,x) => c + x*(x+1)) }, N, "fold" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; range.foreach{ i => s += i*(i+1) }; s }, N, "foreach" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; val i = range.iterator; while (i.hasNext) { val x = i.next; s += x*(x+1) }; s }, N, "iter" ) )
    println( th.pbenchWarm( th.Warm{ var i,s=0; while (i<range.size) { val x = range(i); s += x*(x+1); i += 1 }; s }, N, "loop" ) )

    println("\nlist")
    println( th.pbenchWarm( th.Warm{ list.map(i => i*(i+1)).sum }, N, "map" ) )
    println( th.pbenchWarm( th.Warm{ list.view.map(i => i*(i+1)).sum }, N, "view map" ) )    
    println( th.pbenchWarm( th.Warm{ ( 0 /: list)((c,x) => c + x*(x+1)) }, N, "fold" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; list.foreach{ i => s += i*(i+1) }; s }, N, "foreach" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; val i = list.iterator; while (i.hasNext) { val x = i.next; s += x*(x+1) }; s }, N, "iter" ) )
    def reclsum(xs: List[Int], s: Int = 0): Int = xs match { case x :: more => reclsum(more, s + x*(x+1)); case _ => s }
    println( th.pbenchWarm( th.Warm{ reclsum(list) }, N, "recurse" ) )

    println("\nvect")
    println( th.pbenchWarm( th.Warm{ vect.map(i => i*(i+1)).sum }, N, "map" ) )
    println( th.pbenchWarm( th.Warm{ vect.view.map(i => i*(i+1)).sum }, N, "view map" ) )    
    println( th.pbenchWarm( th.Warm{ ( 0 /: vect)((c,x) => c + x*(x+1)) }, N, "fold" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; vect.foreach{ i => s += i*(i+1) }; s }, N, "foreach" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; val i = vect.iterator; while (i.hasNext) { val x = i.next; s += x*(x+1) }; s }, N, "iter" ) )
    def recvsum(xs: Vector[Int], s: Int = 0): Int = if (xs.length==0) s else recvsum(xs.tail, s + xs.head*(xs.head+1))
    println( th.pbenchWarm( th.Warm{ recvsum(vect) }, N, "recurse" ) )
    println( th.pbenchWarm( th.Warm{ var i,s=0; while (i<range.size) { val x = vect(i); s += x*(x+1); i += 1 }; s }, N, "loop" ) )

    println("\narbuf")
    println( th.pbenchWarm( th.Warm{ arbuf.map(i => i*(i+1)).sum }, N, "map" ) )
    println( th.pbenchWarm( th.Warm{ arbuf.view.map(i => i*(i+1)).sum }, N, "view map" ) )    
    println( th.pbenchWarm( th.Warm{ ( 0 /: arbuf)((c,x) => c + x*(x+1)) }, N, "fold" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; arbuf.foreach{ i => s += i*(i+1) }; s }, N, "foreach" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; val i = arbuf.iterator; while (i.hasNext) { val x = i.next; s += x*(x+1) }; s }, N, "iter" ) )
    println( th.pbenchWarm( th.Warm{ var i,s=0; while (i<arbuf.size) { val x = arbuf(i); s += x*(x+1); i += 1 }; s }, N, "loop" ) )

    println("\narr")
    println( th.pbenchWarm( th.Warm{ arr.map(i => i*(i+1)).sum }, N, "map" ) )
    println( th.pbenchWarm( th.Warm{ arr.view.map(i => i*(i+1)).sum }, N, "view map" ) )    
    println( th.pbenchWarm( th.Warm{ ( 0 /: arr)((c,x) => c + x*(x+1)) }, N, "fold" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; arr.foreach{ i => s += i*(i+1) }; s }, N, "foreach" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; val i = arr.iterator; while (i.hasNext) { val x = i.next; s += x*(x+1) }; s }, N, "iter" ) )
    println( th.pbenchWarm( th.Warm{ var i,s=0; while (i<arr.length) { val x = arr(i); s += x*(x+1); i += 1 }; s }, N, "loop" ) )
    
    println("\nset")
    println( th.pbenchWarm( th.Warm{ set.map(i => i*(i+1)).sum }, N, "map" ) )
    println( th.pbenchWarm( th.Warm{ set.view.map(i => i*(i+1)).sum }, N, "view map" ) )    
    println( th.pbenchWarm( th.Warm{ ( 0 /: set)((c,x) => c + x*(x+1)) }, N, "fold" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; set.foreach{ i => s += i*(i+1) }; s }, N, "foreach" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; val i = set.iterator; while (i.hasNext) { val x = i.next; s += x*(x+1) }; s }, N, "iter" ) )

    println("\nmap")
    println( th.pbenchWarm( th.Warm{ map.map(i => i._1*i._2).sum }, N, "map" ) )
    println( th.pbenchWarm( th.Warm{ map.view.map(i => i._1*i._2).sum }, N, "view map" ) )    
    println( th.pbenchWarm( th.Warm{ ( 0 /: map)((c,x) => c + x._1*x._2) }, N, "fold" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; map.foreach{ i => s += i._1*i._2 }; s }, N, "foreach" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; val i = map.iterator; while (i.hasNext) { val x = i.next; s += x._1*x._2 }; s }, N, "iter" ) )
    
    println("\nbest--string")
    println( th.pbenchWarm( th.Warm{ var s=0; slist.foreach{ s += _.length }; s }, N, "list" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; val i = svect.iterator; while (i.hasNext) { s += i.next.length }; s }, N, "vect" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; slist.foreach{ s += _.length }; s }, N, "arbuf" ) )
    println( th.pbenchWarm( th.Warm{ var i,s=0; while (i<sarr.length) { s += sarr(i).length; i += 1 }; s }, N, "arr" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; sset.foreach{ s += _.length }; s }, N, "set" ) )
    println( th.pbenchWarm( th.Warm{ var s=0; smap.foreach{ s += _._2.length }; s }, N, "map" ) )

    println
    println( th.porder(Thyme.Resource{ i => i })(r => Range(r.start, r.start+r.n).drop(mysteryIndex).head)(1024, "range") )
    println( th.porder(Thyme.Resource{ i => i })(r => List.range(r.start, r.start+r.n).drop(mysteryIndex).head)(1024, "list range") )
    println( th.porder(Thyme.Resource{ i => i })(r => Vector.range(r.start, r.start+r.n).apply(mysteryIndex))(1024, "vector range") )
    println( th.porder(Thyme.Resource{ i => i })(r => ArB.range(r.start, r.start+r.n).apply(mysteryIndex))(1024, "arbuf range") )
    println( th.porder(Thyme.Resource{ i => i })(r => Array.range(r.start, r.start+r.n).apply(mysteryIndex))(1024, "array range") )
    println( th.porder(Thyme.Resource{ i => i })(r => Range(r.start, r.start+r.n).toSet.contains(mysteryIndex+r.start))(1024, "set create") )
    println( th.porder(Thyme.Resource{ i => i })(r => Range(r.start, r.start+r.n).map(x => (x,r.data)).toMap.apply(mysteryIndex))(1024, "map create") )
    
    println
    println( th.porder(Thyme.Resource{ i => T(i+1)(i => Range(0,i)) })(r => r.data(r.n).map(i => (i+1)*i).apply(mysteryIndex))(1024, "range map") )
    println( th.porder(Thyme.Resource{ i => T(i+1)(i => List.range(0,i)) })(r => r.data(r.n).map(i => (i+1)*i).apply(mysteryIndex))(1024, "list map") )
    println( th.porder(Thyme.Resource{ i => T(i+1)(i => Vector.range(0,i)) })(r => r.data(r.n).map(i => (i+1)*i).apply(mysteryIndex))(1024, "vector map") )
    println( th.porder(Thyme.Resource{ i => T(i+1)(i => ArB.range(0,i)) })(r => r.data(r.n).map(i => (i+1)*i).apply(mysteryIndex))(1024, "arbuf map") )
    println( th.porder(Thyme.Resource{ i => T(i+1)(i => ArB.range(0,i)) })(r => r.data(r.n).transform(i => (i+1)*i).apply(mysteryIndex))(1024, "arbuf xform") )
    println( th.porder(Thyme.Resource{ i => T(i+1)(i => Array.range(0,i)) })(r => r.data(r.n).map(i => (i+1)*i).apply(mysteryIndex))(1024, "array map") )
    println( th.porder(Thyme.Resource{ i => T(i+1)(i => Array.range(0,i)) }){ r =>
      var i = 0
      val a = r.data(r.n)
      while (i < a.length) {
        a(i) = (a(i)+1)*a(i)
        i += 1
      }
      r.data(mysteryIndex)
    }(1024, "primitive in-place map") )
    println( th.porder(Thyme.Resource{ i => T(i+1)(i => Range(0,i).toSet) })(r => r.data(r.n).map(i => (i+1)*i).contains(mysteryIndex))(1024, "set map") )
    println( th.porder(Thyme.Resource{ i => T(i+1)(j => Range(0,j).map(j => (j,i)).toMap) }){ r =>
      r.data(r.n).map{ x => (x._1,(x._2+1)*x._1) }.apply(mysteryIndex)
    }(1024, "map map") )
    
    println
    println( th.porder(Thyme.Resource{ i => T(i+1)(i => Range(0,i)) })(r => (0 /: r.data(r.n))(_ * _ + 1))(1024, "range fold") )
    println( th.porder(Thyme.Resource{ i => T(i+1)(i => List.range(0,i)) })(r =>(0 /: r.data(r.n))(_ * _ + 1))(1024, "list fold") )
    println( th.porder(Thyme.Resource{ i => T(i+1)(i => Vector.range(0,i)) })(r => (0 /: r.data(r.n))(_ * _ + 1))(1024, "vector fold") )
    println( th.porder(Thyme.Resource{ i => T(i+1)(i => ArB.range(0,i)) })(r => (0 /: r.data(r.n))(_ * _ + 1))(1024, "arbuf fold") )
    println( th.porder(Thyme.Resource{ i => T(i+1)(i => Array.range(0,i)) })(r => (0 /: r.data(r.n))(_ * _ + 1))(1024, "array fold") )
    println( th.porder(Thyme.Resource{ i => T(i+1)(i => Array.range(0,i)) }){ r =>
      var i,s = 0
      val a = r.data(r.n)
      while (i < a.length) {
        s = (s*a(i))+1
        i += 1
      }
      s
    }(1024, "primitive manual fold") )
    println( th.porder(Thyme.Resource{ i => T(i+1)(i => Range(0,i).toSet) })(r => (0 /: r.data(r.n))(_ * _ + 1))(1024, "set fold") )
    println( th.porder(Thyme.Resource{ i => T(i+1)(j => Range(0,j).map(j => (j,i)).toMap) }){ r => (0 /: r.data(r.n))(_ * _._2 + 1) }(1024, "map fold") )
  }
}
