
```scala
package ohnosequences.fastarious.test

import org.scalatest.FunSuite

import ohnosequences.cosas._, types._, klists._
import ohnosequences.fastarious._, fasta._
import scala.collection.JavaConversions._
import java.nio.file._
import java.io._


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

    val ls = f.asString
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

    val fastaFile = new File("test.fasta")
    Files.deleteIfExists(fastaFile.toPath)

    val id = FastaHeader("id|12312312 una secuencia cualquiera")
    val randomLines = FastaSequence("ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATCCCCCCTTCTACACTCCCCCCCCCCCACATGGTCATTTCTACACACCCCCCCCCCCCCCCCGGGGGGGGGGGGGGGGGGGGGGGGGGGCATCCCTACATATACTTCTCGTCATACTCATACATACACCCCCCCCCCCACAGGGGTCCATACAAAGGGCTTATATCCCCACGGGTCTTTTTCACTTCATATTTTTGGGGGCCTCGCGCGCCCTTAC")

    // somewhere around 2MB
    val l = FASTA(
      (header   := id)           ::
      (sequence := randomLines)  ::
      *[AnyDenotation]
    )

    val fastas = Iterator.fill(10000)(l)

    fastas appendTo fastaFile
  }

  test("parsing from iterator") {

    val fastaFile   = new File("test.fasta")
    val parsedFile  = new File("parsed.fasta")
    Files.deleteIfExists(parsedFile.toPath)

    import scala.collection.JavaConversions._

    // WARNING this will leak file descriptors
    val lines   = Files.lines(fastaFile.toPath).iterator
    val asFasta = lines.buffered.parseFastaDropErrors()

    asFasta appendTo parsedFile
  }

  test("FASTA lines parsing") {

    val crap = Seq(
      "hola fasta! lalala",
      "oh no, no soy fasta"
    )

    val fasta1 = FASTA(
      header(FastaHeader(">1 hola")) ::
      sequence(FastaSequence("ATCACCCACTTTACATTTCACACACCCCTTTACAC")) ::
      *[AnyDenotation]
    )

    val fasta2 = FASTA(
      header(FastaHeader(">2 hola")) ::
      sequence(FastaSequence("ATATACCCACACCCCGGTCAT")) ::
      *[AnyDenotation]
    )

    val emptyFasta = FASTA(
      header(FastaHeader(">3 nothing")) ::
      sequence(FastaSequence("")) ::
      *[AnyDenotation]
    )

    assert {
      (crap ++ fasta1.lines).iterator.buffered.parseFastaDropErrors().toList ==
        List(fasta1)
    }

    assert {
      (crap ++ emptyFasta.lines ++ fasta1.lines).iterator.buffered.parseFastaDropErrors().toList ==
        List(emptyFasta, fasta1)
    }

    assert {
      emptyFasta.lines.iterator.buffered.parseFastaDropErrors().toList ==
        List(emptyFasta)
    }

    assert {
      (fasta1.lines ++ emptyFasta.lines).iterator.buffered.parseFastaDropErrors().toList ==
        List(fasta1, emptyFasta)
    }

    assert {
      List(fasta1,fasta2).flatMap(_.lines).iterator.buffered.parseFastaDropErrors().toList ==
        List(fasta1,fasta2)
    }

    assert {
      List(emptyFasta,fasta2,fasta1,fasta2,fasta2,fasta1).flatMap(_.lines).iterator.buffered.parseFastaDropErrors().toList ==
        List(emptyFasta,fasta2,fasta1,fasta2,fasta2,fasta1)
    }

    assert {
      List(emptyFasta,emptyFasta,fasta2,emptyFasta,fasta1,fasta2,fasta2,fasta1,emptyFasta).flatMap(_.lines).iterator.buffered.parseFastaDropErrors().toList ==
        List(emptyFasta,emptyFasta,fasta2,emptyFasta,fasta1,fasta2,fasta2,fasta1,emptyFasta)
    }
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