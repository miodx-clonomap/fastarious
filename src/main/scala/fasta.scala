package ohnosequences.fastarious

import ohnosequences.cosas._, types._, properties._, records._

case object fasta {

  case object header    extends Property[String]("fasta.header") { val start = ">" }
  case object sequence  extends Property[FastaLines]("fasta.sequence")

  type FASTA = FASTA.type
  case object FASTA extends Record(
    header    :&:
    sequence  :&: □
  )
  {
    implicit def fastaOps(fa: FASTA.type := FASTA.Raw): FASTAOps = new FASTAOps(fa.value)
  }

  implicit lazy val serializeHeader =
    PropertySerializer(header, header.label){ v: String => Some(s"${header.start}${v}") }

  case object FastaLines {

    def apply(ll: Seq[String]): FastaLines =
      new FastaLines( ll map { _.filterNot(c => c equals '\n') } flatMap { _.grouped(70) } )
  }

  final class FastaLines private(val lines: Seq[String]) extends AnyVal {

    def ++(other: FastaLines): FastaLines = FastaLines(lines ++ other.lines)
  }

  final class FASTAOps(val fa: FASTA.Raw) extends AnyVal {

    @inline private def me: ValueOf[FASTA.type] = FASTA(fa)

    def toLines: Seq[String] =
      Seq(s"${header.start}${me get header value}") ++ ((me get sequence value) lines)
  }

}
