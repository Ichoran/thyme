// Maths.scala Copyright Rex Kerr 2013 / BSD 2-clause

import ichi.bench.Thyme

object Maths {
  final val N = 1000
  @volatile var mysteryIndex = 0
  var _i = 0
  var _d = 0.0

  def base = { var i,s = 0; while (i<N) { s += (i+1)*i; i += 1 }; s }
  def assign = { var i,s = 0; while (i<N) { val x = (i+1)*i; _i = x; s += x; i += 1 }; s }
  def plusConst = { var i,s = 0; while (i<N) { s += (i+1)*i + 2; i += 1 }; s }
  def plusVar = { var i,s = 0; while (i<N) { s += (i+1)*i + i; i += 1 }; s }
  def bitsConst = { var i,s = 0; while (i<N) { s += (i+1)*(i&2); i += 1 }; s }
  def bitsVar = { var i,s = 0; while (i<N) { s += (i+1)*(i&s); i += 1 }; s }
  def timesConst = { var i,s = 0; while (i<N) { s += (i+1)*i * 3; i += 1 }; s }
  def timesVar = { var i,s = 0; while (i<N) { s += (i+1)*i*i; i += 1 }; s }
  def divConst = { var i,s = 0; while (i<N) { s += ((i+1)*i)/3; i += 1 }; s }
  def divVar = { var i,s = 0; while (i<N) { s += i*i/(s+1); i += 1 }; s }
  
  def baseD = { var i,s = 0.5; while (i<N) { s += (i+1)*i; i += 1 }; s }
  def assignD = { var i,s = 0.5; while (i<N) { val x = (i+1)*i; _d = x; s += x; i += 1 }; s }
  def plusConstD = { var i,s = 0.5; while (i<N) { s += (i+1)*i + 2.0; i += 1 }; s }
  def plusVarD = { var i,s = 0.5; while (i<N) { s += (i+1)*i + i; i += 1 }; s }
  def timesConstD = { var i,s = 0.5; while (i<N) { s += (i+1)*i*3.0; i += 1 }; s }
  def timesVarD = { var i,s = 0.5; while (i<N) { s += (i+1)*i*i; i += 1 }; s }
  def divConstD = { var i,s = 0.5; while (i<N) { s += (i+1)*i/3.0; i += 1 }; s }
  def divVarD = { var i,s = 0.5; while (i<N) { s += i*i/(s+1); i += 1 }; s }
  def sqrtD = { var i,s = 0.5; while (i<N) { s += math.sqrt((i+1)*i); i += 1 }; s }
  def logD = { var i,s = 0.5; while (i<N) { s += math.log((i+1)*i); i += 1 }; s }
  def powD = { var i,s = 0.5; while (i<N) { s += math.pow((i+1)*i,0.3); i += 1 }; s }
  def sinD = { var i,s = 0.5; while (i<N) { s += math.sin((i+1)*i); i += 1 }; s }
  
  def plusBI = { var i = 0; var s = BigInt(0); while (i<N) { s += (i+1)*i; i += 1 }; s }
  def plusTimesBI = { var i,s = BigInt(0); while (i<N) { s += (i+1)*i; i += 1 }; s }
  def plusBD = { var i = 0; var s = BigDecimal(0.0); while (i<N) { s += (i+1)*i; i += 1 }; s }
  def plusTimesBD = { var i,s = BigDecimal(0.0); while (i<N) { s += (i+1)*i; i += 1 }; s }
  def divBI = { var i,s = BigInt(0); while (i<N) { s += ((i+1)*i)/3; i += 1 }; s }
  
  def bigplus(a: BigInt, b: BigInt) = a+b
  def bigtimes(a: BigInt, b: BigInt) = a*b
  def bigdiv(a: BigInt, b: BigInt) = a/b
  
  def main(args: Array[String]) {
    val th = Thyme.warmed(verbose = print)
    th.pbenchWarm( th.Warm{base}, N, "base" )
    th.pbenchWarm( th.Warm{assign}, N, "=" )
    th.pbenchWarm( th.Warm{plusConst}, N, "+2" )
    th.pbenchWarm( th.Warm{plusVar}, N, "+i" )
    th.pbenchWarm( th.Warm{bitsConst}, N, "&2" )
    th.pbenchWarm( th.Warm{bitsVar}, N, "&s" )
    th.pbenchWarm( th.Warm{timesConst}, N, "*3" )
    th.pbenchWarm( th.Warm{timesVar}, N, "*i" )
    th.pbenchWarm( th.Warm{divConst}, N, "/3" )
    th.pbenchWarm( th.Warm{divVar}, N, "/s" )
    
    println
    th.pbenchWarm( th.Warm{baseD}, N, "base" )
    th.pbenchWarm( th.Warm{assignD}, N, "=" )
    th.pbenchWarm( th.Warm{plusConstD}, N, "+2" )
    th.pbenchWarm( th.Warm{plusVarD}, N, "+i" )
    th.pbenchWarm( th.Warm{timesConstD}, N, "*3" )
    th.pbenchWarm( th.Warm{timesVarD}, N, "*i" )
    th.pbenchWarm( th.Warm{divConstD}, N, "/3" )
    th.pbenchWarm( th.Warm{divVarD}, N, "/s" )
    th.pbenchWarm( th.Warm{sqrtD}, N, "sqrt" )
    th.pbenchWarm( th.Warm{logD}, N, "log" )
    th.pbenchWarm( th.Warm{powD}, N, "pow" )
    th.pbenchWarm( th.Warm{sinD}, N, "sin" )
    
    println
    val a = BigInt("9428759129051209612847928759817049175389275089179017209347190274589012745917295172907510940987654321")
    val b = BigInt("82370834968209759172035748902735091873289047102758")
    val c = BigInt("9428759129051209612847928759817049175389275089179017209347190274589012745917295172907510940987654321"*10)
    val d = BigInt("82370834968209759172035748902735091873289047102758"*10)
    th.pbenchWarm( th.Warm{plusBI}, N, "+" )
    th.pbenchWarm( th.Warm{plusTimesBI}, N, "*" )
    th.pbenchWarm( th.Warm{divBI}, N, "/" )
    th.pbenchWarm( th.Warm{bigplus(a,b)}, 1, "big +" )
    th.pbenchWarm( th.Warm{bigtimes(a,b)}, 1, "big *" )
    th.pbenchWarm( th.Warm{bigdiv(a,b)}, 1, "big /" )
    th.pbenchWarm( th.Warm{bigplus(c,d)}, 1, "huge +" )
    th.pbenchWarm( th.Warm{bigtimes(c,d)}, 1, "huge *" )
    th.pbenchWarm( th.Warm{bigdiv(c,d)}, 1, "huge /" )
    
    println
    th.pbenchWarm( th.Warm{plusBD}, N, "+" )
    th.pbenchWarm( th.Warm{plusTimesBD}, N, "*" )
  }
}