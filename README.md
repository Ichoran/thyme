# Thyme (and Parsley)

Thyme and Parsley are microbenchmarking and profiling utilities.

This is version 0.1.2.

A couple of things to keep in mind before you start:
  (1) Context is important!  (Especially for tricky JIT compilation and GC.)
  (2) The JIT will skip work if it can--always return an answer in a
      microbenchmark, and make it depend on everything you do.

If you want to build the project, use sbt.

The makefile probably works, but it's not kept up-to-date any more.
If you do try to use it, just compile everything in maths
together and then everything in bench--that works fine.  If you're on
a Unixy system, there's makefile which works also (and builds the docs).

If you use sbt, in sbt just type "run" to compile then you will will
be offered a list of the included examples to run.

There are no dependencies save Scala 2.12 itself.

### Maven Dependency

Thyme is available now as a snapshot.  Add

```
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
```

to the top of your `build.sbt`.

Then use the following dependency:

```
"com.github.ichoran" %% "thyme" % "0.1.2-SNAPSHOT"
```

Here is a complete build file that will get Thyme for you and allow you to load the console:

```
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

lazy val root = (project in file(".")).settings(
  name := "thyme-test",
  version := "0.1.0",
  scalaVersion := "2.12.0",
  libraryDependencies += "com.github.ichoran" %% "thyme" % "0.1.2-SNAPSHOT"
)
```

You can then type `sbt console` and you should see something like this:

```
[info] Loading global plugins from /(your-home)/.sbt/0.13/plugins
[info] Set current project to thyme-test (in build file:/(path-where-build.sbt-is)/)
[info] Updating {file:/(path-where-build.sbt-is)/}root...
[info] Resolving jline#jline;2.14.1 ...
[info] downloading https://oss.sonatype.org/content/repositories/snapshots/com/github/ichoran/thyme_2.12/0.1.2-SNAPSHOT/thyme_2.12-0.1.2-SNAPSHOT.jar ...
[info]  [SUCCESSFUL ] com.github.ichoran#thyme_2.12;0.1.2-SNAPSHOT!thyme_2.12.jar (1263ms)
[info] Done updating.
[info] Starting scala interpreter...
[info] 
Welcome to Scala 2.12.0 (OpenJDK 64-Bit Server VM, Java 1.8.0_111).
Type in expressions for evaluation. Or try :help.

scala>
```

### But I use Scala 2.10 (or 2.11)!!!

The historical Thyme.jar is included.  Why not try it out!  You can do it right in the
REPL, like so (assuming you have 2.10 or 2.11 installed to load from the command-line):

```
$ scala -cp Thyme.jar
Welcome to Scala version 2.10.1 (Java HotSpot(TM) 64-Bit Server VM, Java
1.6.0_37).
Type in expressions to have them evaluated.
Type :help for more information.
```

## Thyme

Either by using the REPL with Thyme.jar, or by using the console in SBT
with the correct dependency, you have gotten to a `scala>` prompt.

You can do things like so:

```
scala> // If you don't want to wait for warmup...
scala> // val th = new ichi.bench.Thyme
scala> // ...will give pretty decent/accurate results also.

scala> val th = ichi.bench.Thyme.warmed(verbose = print)
Creating Thyme instances and warming busywork methods...done in 2.81 s
Warming up benchmarking...done in 4.77 s
Warming up head-to-head benchmarking...done in 4.22 s
Warming up computational complexity benchmarking...done in 4.39 s
th: ichi.bench.Thyme = ichi.bench.Thyme@626287d3

scala> th.pbench((1 to 1000).map(i => i*i).sum)
Benchmark (2540 calls in 189.8 ms)
  Time:    46.79 us   95% CI 45.98 us - 47.60 us   (n=18)
  Garbage: 12.50 us   (n=3 sweeps measured)
res0: Int = 333833500

scala> val w = th.Warm(Array.range(1,1001).map(i => i*i).sum)
w: th.Warm[Int] = ichi.bench.Thyme$Warm@3f97171f

scala> th.pbenchWarm(w)
Benchmark (60 calls in 614.3 ms)
  Time:    31.82 us   95% CI 31.27 us - 32.38 us   (n=19)
  Garbage: 1.758 us   (n=11 sweeps measured)
res1: Int = 333833500

scala> th.pbenchOffWarm()(th.Warm((1 to 1000).map(i => i*i).sum))(w)
Benchmark comparison (in 2.730 s)
Significantly different (p ~= 0)
  Time ratio:    1.19718   95% CI 1.18902 - 1.20533   (n=20)
    First     26.10 us   95% CI 26.01 us - 26.19 us
    Second    31.24 us   95% CI 31.06 us - 31.43 us
res2: Int = 333833500

scala> :quit
```

## Parsley

Parsley is a local profiler that also can be useful.  Try this, then check
the source code of bench/Examples.scala to see what's happening (tldr--it
measures the relative time taken by three different methods as they do
they're thing embedded in some other code):

```
$ scala -cp Thyme.jar ichi.bench.examples.ParsleyExample
Parsley profiling, 3564 ticks sampled
Fractional time taken: 
   46.971% +- 0.836%   = Method 2
   26.192% +- 0.736%   = Method 3
   23.584% +- 0.711%   = Method 1
Relative time taken per entry (observed entries only): 
  100.000% +- 2.516%   = Method 2
   55.834% +- 1.857%   = Method 3
   50.544% +- 1.769%   = Method 1

Parsley profiling, 173 ticks sampled
Fractional time taken: 
    0.571% +- 0.568%   = one
    0.571% +- 0.568%   = two
    0.571% +- 0.568%   = three

Parsley profiling, 3524 ticks sampled
Fractional time taken: 
   46.852% +- 0.840%   = Method 2
   26.149% +- 0.740%   = Method 1
   26.092% +- 0.739%   = Method 3

Method 1
  Elapsed time: 2.910 s
  Garbage collection (4 sweeps) took: 13. ms
  0 classes were loaded
Method 2
  Elapsed time: 5.576 s
  Garbage collection (4 sweeps) took: 9. ms
  0 classes were loaded
Method 3
  Elapsed time: 3.054 s
  Garbage collection (4 sweeps) took: 6. ms
  0 classes were loaded
$
```

## Changelog

#### 0.1.2

1. Upgraded to Scala 2.12.
2. Deployed to a Maven repository (via Sonatype).

#### 0.1.1

1. Added argument to allow Thyme.warmed to partially warm an
instance.
