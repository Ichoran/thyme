default: \
  Thyme.jar \
  Thyme-docs.tgz

MATHS = \
  ichi/maths/Rng.class \
  ichi/maths/PackedMaths.class \
  ichi/maths/package.class

BENCHES = \
  ichi/bench/JvmStatus.class \
  ichi/bench/Thyme.class \
  ichi/bench/Parsley.class

EXAMPLES = \
  ichi/bench/examples/ParsleyExample.class \
  ichi/bench/examples/ThymeExample.class

Thyme-docs.tgz : \
  Thyme.jar
	mkdir -p docs && scaladoc -cp Thyme.jar -d docs bench/*.scala && tar czf Thyme-docs.tgz docs/

Thyme.jar : \
  ${MATHS} \
  ${BENCHES} \
  ${EXAMPLES}
	jar cf Thyme.jar ichi/

ichi/maths/Rng.class : \
  makefile \
  maths/Rng.scala
	scalac maths/Rng.scala

ichi/maths/PackedMaths.class : \
  makefile \
  maths/PackedMaths.scala
	scalac maths/PackedMaths.scala

ichi/maths/package.class : \
  makefile \
  ichi/maths/Rng.class \
  ichi/maths/PackedMaths.class \
  maths/package.scala
	rm ichi/maths/package*.class; scalac maths/package.scala

ichi/bench/JvmStatus.class : \
  makefile \
  ${MATHS} \
  bench/JvmStatus.scala
	scalac bench/JvmStatus.scala

ichi/bench/Thyme.class : \
  makefile \
  ${MATHS} \
  ichi/bench/JvmStatus.class \
  bench/Thyme.scala
	scalac bench/Thyme.scala

ichi/bench/Parsley.class : \
  makefile \
  ${MATHS} \
  ichi/bench/JvmStatus.class \
  ichi/bench/Thyme.class \
  bench/Parsley.scala
	scalac bench/Parsley.scala

ichi/bench/examples/ParsleyExample.class : \
  makefile \
  ${MATHS} \
  ${BENCHES} \
  bench/Examples.scala
	scalac bench/Examples.scala

ichi/bench/examples/ThymeExample.class : \
  makefile \
  ${MATHS} \
  ${BENCHES} \
  bench/Examples.scala
	scalac bench/Examples.scala


clean:
	rm ichi/bench/examples/*.class; rm ichi/bench/*.class; rm ichi/maths/*.class; rm -r docs/ichi
