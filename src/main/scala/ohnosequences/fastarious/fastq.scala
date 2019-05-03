package ohnosequences.fastarious

import fasta._
import java.io._

case object fastq {

  /*
    Here `value` is assumed to correspond to the real id, *without* the `@` character which would identify it in its serialized form. So, do *not* pass `"@HWI:2321"`, write instead `Id("HWI:2321")`
  */
  case class Id(val value: String) extends AnyVal {

    def asString: String =
      s"@${value.filterNot(_ == '\n')}"

    def isEmpty: Boolean =
      value.isEmpty
  }

  case object Id {

    /*
      ### Id parsing

      These methods are *not* for building Id values; for that use `from`.
    */
    private val isValid: String => Boolean =
      _ startsWith "@"

    /*
      `parseFrom` will drop the *first* '@' in `raw`, if any. My understanding is that `@@hola` is a valid (albeit confusing) FASTQ id.
    */
    def parseFrom(raw: String): Option[Id] =
      if(isValid(raw)) Some( new Id(raw.stripPrefix("@")) ) else None
  }

  case class FASTQ(val id: Id, val sequence: SequenceQuality) {

    def asStringPhred33: String = Seq(
      id.asString,
      sequence.asStringPhred33
    ).mkString("\n")

    def toFASTA: FASTA = FASTA(
      Header(id.value),
      sequence.sequence
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
