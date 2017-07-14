
```scala
package ohnosequences.fastarious.test

import org.scalatest.FunSuite
import ohnosequences.fastarious._, Quality._

class QualityScores extends FunSuite {

  val r = scala.util.Random

  val SIZE = 1000000
  val DELTA: Num = 0.00001

  val bigQual =
    Quality(Seq.tabulate(SIZE)({ i => r.nextInt(1000) }))
    // Quality(Seq.tabulate(10000)({ i => r.nextInt(1000) }))

  val bigQualSame =
    Quality(Seq.tabulate(SIZE)({ i => 3000 }))

  val bigQualSame2 =
    Quality(Seq.tabulate(SIZE)({ i => 5 }))

  test("error probability calculations") {

    val score1 = 30
    val score2 = 31

    assert { score1.asPhredScore.errorProbability != score2.asPhredScore.errorProbability }
    assert { score1.asPhredScore.errorProbability === 0.001 }

    assert { score2.asPhredScore.errorProbability + score2.asPhredScore.successProbability === 1 }

    val quals =
      Quality( Seq.fill(10)(31) )

    assert { quals.expectedErrors === errorProbability(31)*10 }
    assert { quals.maxScore === quals.minScore }
  }

  test("Big quality errorP and score is idempotent") {

    val eps = bigQual.errorPs
    val qsAgain = eps map scoreFrom

    assert { bigQual === Quality(qsAgain) }
  }

  test("Expected errors") {

    val ee = bigQual.expectedErrors
  }

  test("rounding errors OK for expected errors") {

    assert { Math.abs( 3000.asPhredScore.errorProbability * SIZE - bigQualSame.expectedErrors) < DELTA }
    assert { Math.abs( 5.asPhredScore.errorProbability * SIZE - bigQualSame2.expectedErrors) < DELTA }
  }


}

```




[test/scala/DNA.scala]: DNA.scala.md
[test/scala/NcbiHeadersTests.scala]: NcbiHeadersTests.scala.md
[test/scala/FastqTests.scala]: FastqTests.scala.md
[test/scala/FastaTests.scala]: FastaTests.scala.md
[test/scala/QualityScores.scala]: QualityScores.scala.md
[main/scala/DNAQ.scala]: ../../main/scala/DNAQ.scala.md
[main/scala/quality.scala]: ../../main/scala/quality.scala.md
[main/scala/DNA.scala]: ../../main/scala/DNA.scala.md
[main/scala/package.scala]: ../../main/scala/package.scala.md
[main/scala/fasta.scala]: ../../main/scala/fasta.scala.md
[main/scala/fastq.scala]: ../../main/scala/fastq.scala.md
[main/scala/SequenceQuality.scala]: ../../main/scala/SequenceQuality.scala.md
[main/scala/utils.scala]: ../../main/scala/utils.scala.md
[main/scala/sequence.scala]: ../../main/scala/sequence.scala.md
[main/scala/ncbiHeaders.scala]: ../../main/scala/ncbiHeaders.scala.md