package ohnosequences.fastarious.test

import org.scalatest.FunSuite

import ohnosequences.cosas._, types._, klists._
import ohnosequences.fastarious._, fasta._
import better.files._


class FastaTests extends FunSuite {

  test("can create FASTA values") {

    val f = FASTA(
      header(FastaHeader(">@HUGHA5.ADFDA#")) ::
      sequence(FastaSequence("ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATC")) :: *[AnyDenotation]
    )
  }

  test("can serialize and parse FASTA values") {

    val h = ">@HUGHA5.ADFDA#"
    val seq = """
    ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATC
    ATCCACGA
    TTTCACAACAGTGTCAACTGAACACACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    CCCTACATATAATATATATATACCCGA
    CCCCCTTCTACACTCCCCCCCCCCCACATGGTCATAC
    AACT
    """

    val f = FASTA(
      header(FastaHeader(h))        ::
      sequence(FastaSequence(seq))  ::
      *[AnyDenotation]
    )

    assert {
      f.serialize[String].fold(l => l, r => FASTA parse r) === Right(f)
    }
  }

  test("line length is always 70") {

    val h = ">@HUGHA5.ADFDA#"
    val seq = "ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATCATCCACGATTTCACAACAGTGTCAACTGAACACACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCTACATATAATATATATATACCCGACCCCCTTCTACACTCCCCCCCCCCCACATGGTCATACAACT"

    val f = FASTA(
      header(FastaHeader(h))        ::
      sequence(FastaSequence(seq))  ::
      *[AnyDenotation]
    )

    val ls = f.toLines
    val lsSplit = ls.split('\n')

    assert { lsSplit.filter(l => (l.length <= 70) || l.startsWith(">")) === lsSplit }
  }

  test("id and description == header value") {

    val fa = FASTA(
      header( FastaHeader("adsfa12312 que bonita secuencia") )  ::
      sequence( FastaSequence("AATATAT ATA TACACAC AAATC"))     ::
      *[AnyDenotation]
    )

    assert { s"${fa.getV(header).id}${fa.getV(header).description}" === fa.getV(header).value }
  }



  test("generate fasta file") {

    val fastaFile = file"test.fasta"
    fastaFile.clear

    val id = FastaHeader("id|12312312 una secuencia cualquiera")
    val randomLines = FastaSequence("ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATCCCCCCTTCTACACTCCCCCCCCCCCACATGGTCATTTCTACACACCCCCCCCCCCCCCCCGGGGGGGGGGGGGGGGGGGGGGGGGGGCATCCCTACATATACTTCTCGTCATACTCATACATACACCCCCCCCCCCACAGGGGTCCATACAAAGGGCTTATATCCCCACGGGTCTTTTTCACTTCATATTTTTGGGGGCCTCGCGCGCCCTTAC")

    // somewhere around 2MB
    val l = FASTA(
      (header   := id)           ::
      (sequence := randomLines)  ::
      *[AnyDenotation]
    )
    .toLines

    for(i <- 1 to 100000) { fastaFile.append(l); fastaFile.appendNewLine  }
  }

  test("parsing from iterator") {

    val fastaFile   = file"test.fasta"
    val parsedFile  = file"parsed.fasta"
    parsedFile.clear

    import java.nio.file._
    import scala.collection.JavaConversions._

    val lines   = Files.lines(fastaFile.path).iterator
    val asFasta = fasta.parseFastaFromLines(lines) map {
      case Right(fa) => fa
    }

    asFasta appendTo parsedFile
  }

  test("raw parsing from iterator") {

    val fastaFile   = file"test.fasta"
    val parsedFile  = file"parsed-raw.fasta"
    parsedFile.clear

    val lines   = fastaFile.lineIterator
    val asMaps  = fasta.parseMapFromLines(lines)

    asMaps.foreach {
      map => {
        parsedFile.appendLines(s">${map("header")}")
        parsedFile.appendLines(map("sequence").grouped(70).mkString("\n"))
      }
    }

  }
}
