
```scala
package ohnosequences.fastarious

import fasta._
import ohnosequences.cosas._, types._, records._, fns._, klists._
import java.io._

case object fastq {
```


Here `value` is assumed to correspond to the real id, *without* the `@` character which would identify it in its serialized form. So, do *not* pass `"@HWI:2321"`, write instead `Id("HWI:2321")`


```scala
  case class Id(val value: String) extends AnyVal {

    def asString: String =
      s"@${value.filterNot(_ == '\n')}"

    def isEmpty: Boolean =
      value.isEmpty
  }

  case object Id {
```


### Id parsing

These methods are *not* for building Id values; for that use `from`.


```scala
    private val isValid: String => Boolean =
      _ startsWith "@"
```


`parseFrom` will drop the *first* '@' in `raw`, if any. My understanding is that `@@hola` is a valid (albeit confusing) FASTQ id.


```scala
    def parseFrom(raw: String): Option[Id] =
      if(isValid(raw)) Some( new Id(raw.stripPrefix("@")) ) else None
  }

  case class FASTQ(val id: Id, val sequence: SequenceQuality) {

    def asStringPhred33: String = Seq(
      id.asString,
      sequence.asStringPhred33
    ).mkString("\n")

    def toFASTA: FASTA.Value = FASTA(
      fasta.header( FastaHeader(id.value) )             ::
      fasta.sequence( FastaSequence(sequence.sequence.letters) ) ::
      *[AnyDenotation]
    )

    def updateSequence(f: SequenceQuality => SequenceQuality): FASTQ =
      this.copy(sequence = f(this.sequence))
  }

  case object FASTQ {

    def fromStringsPhred33(
      id      : String,
      letters : String,
      quality : String
    )
    : Option[FASTQ] =
      SequenceQuality.fromStringsPhred33(letters, quality) map { FASTQ( Id(id), _ ) }
  }

  implicit class FASTQIteratorOps(val fastqs: Iterator[FASTQ]) extends AnyVal {

    def appendAsPhred33To(file: File): File = {

      if(fastqs.hasNext) {
        val wr = new BufferedWriter(new FileWriter(file, true))

        fastqs.foreach { fq =>
          wr.write( fq.asStringPhred33 )
          wr.newLine
        }
        wr.close
      }

      file
    }
  }

  implicit class IteratorFASTQOps(val lines: Iterator[String]) extends AnyVal {

    def parseFastqPhred33: Iterator[Option[FASTQ]] =
      lines
        .grouped(4).filter(_.size == 4) // grouped is broken
        .map { quartet =>

          if( quartet(2) startsWith "+" ) {
            for {
              i <- Id.parseFrom( quartet(0) )
              s <- SequenceQuality.fromStringsPhred33( quartet(1), quartet(3) )
            } yield FASTQ(i, s)
          }
          else None
        }

    def parseFastqPhred33DropErrors: Iterator[FASTQ] =
      parseFastqPhred33 collect { case Some(fq) => fq }
  }
}

```




[test/scala/DNA.scala]: ../../test/scala/DNA.scala.md
[test/scala/NcbiHeadersTests.scala]: ../../test/scala/NcbiHeadersTests.scala.md
[test/scala/FastqTests.scala]: ../../test/scala/FastqTests.scala.md
[test/scala/FastaTests.scala]: ../../test/scala/FastaTests.scala.md
[test/scala/QualityScores.scala]: ../../test/scala/QualityScores.scala.md
[main/scala/DNAQ.scala]: DNAQ.scala.md
[main/scala/quality.scala]: quality.scala.md
[main/scala/DNA.scala]: DNA.scala.md
[main/scala/package.scala]: package.scala.md
[main/scala/fasta.scala]: fasta.scala.md
[main/scala/fastq.scala]: fastq.scala.md
[main/scala/SequenceQuality.scala]: SequenceQuality.scala.md
[main/scala/utils.scala]: utils.scala.md
[main/scala/sequence.scala]: sequence.scala.md
[main/scala/ncbiHeaders.scala]: ncbiHeaders.scala.md