package ohnosequences.fastarious.test

import org.scalatest.FunSuite
import ohnosequences.fastarious._, fasta._
import java.nio.file._
import java.io._

class FastaTests extends FunSuite {

  def testFasta(): File = new File(
    this.getClass.getResource("/test.fasta").getPath
  )

  def testOut(): File = {
    val out = new File("target/test-out.fasta")
    Files.deleteIfExists(out.toPath)
    out
  }

  test("line length is always 70") {

    val h = ">@HUGHA5.ADFDA#"
    val seq = "ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATCATCCACGATTTCACAACAGTGTCAACTGAACACACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCTACATATAATATATATATACCCGACCCCCTTCTACACTCCCCCCCCCCCACATGGTCATACAACT"

    val f = FASTA(
      Header(h),
      Sequence(seq)
    )

    val ls = f.asString
    val lsSplit = ls.split('\n')

    assert { lsSplit.filter(l => (l.length <= 70) || l.startsWith(">")) === lsSplit }
  }

  test("id and description == header value") {

    val fa = FASTA(
      Header("adsfa12312 que bonita secuencia"),
      Sequence("AATATAT ATA TACACAC AAATC")
    )

    assert { s"${fa.header.id} ${fa.header.description}" === fa.header.value }
  }



  test("generate fasta file") {
    val out = testOut()

    val id = Header("id|12312312 una secuencia cualquiera")
    val randomLines = Sequence("ATCCGTCCGTCCTGCGTCAAACGTCTGACCCACGTTTGTCATCATCCCCCCTTCTACACTCCCCCCCCCCCACATGGTCATTTCTACACACCCCCCCCCCCCCCCCGGGGGGGGGGGGGGGGGGGGGGGGGGGCATCCCTACATATACTTCTCGTCATACTCATACATACACCCCCCCCCCCACAGGGGTCCATACAAAGGGCTTATATCCCCACGGGTCTTTTTCACTTCATATTTTTGGGGGCCTCGCGCGCCCTTAC")

    // somewhere around 2MB
    val l = FASTA(id, randomLines)

    val fastas = Iterator.fill(10000)(l)

    fastas appendTo out
  }

  test("parsing from iterator") {
    val in  = testFasta()
    val out = testOut()

    lines(in).buffered.parseFasta appendTo out

    assert { lines(in).toList == lines(out).toList }
  }

  test("FASTA lines parsing") {

    val crap = Seq(
      "hola fasta! lalala",
      "oh no, no soy fasta"
    )

    val fasta1 = FASTA(
      Header(">1 hola"),
      Sequence("ATCACCCACTTTACATTTCACACACCCCTTTACAC")
    )

    val fasta2 = FASTA(
      Header(">2 hola"),
      Sequence("ATATACCCACACCCCGGTCAT")
    )

    val emptyFasta = FASTA(
      Header(">3 nothing"),
      Sequence("")
    )

    assert {
      (crap ++ fasta1.lines).iterator.buffered.parseFastaSkipCrap.toList ==
        List(fasta1)
    }

    assert {
      (crap ++ emptyFasta.lines ++ fasta1.lines).iterator.buffered.parseFastaSkipCrap.toList ==
        List(emptyFasta, fasta1)
    }

    assert {
      emptyFasta.lines.iterator.buffered.parseFasta.toList ==
        List(emptyFasta)
    }

    assert {
      (fasta1.lines ++ emptyFasta.lines).iterator.buffered.parseFasta.toList ==
        List(fasta1, emptyFasta)
    }

    assert {
      List(fasta1,fasta2).flatMap(_.lines).iterator.buffered.parseFasta.toList ==
        List(fasta1,fasta2)
    }

    assert {
      List(emptyFasta,fasta2,fasta1,fasta2,fasta2,fasta1).flatMap(_.lines).iterator.buffered.parseFasta.toList ==
        List(emptyFasta,fasta2,fasta1,fasta2,fasta2,fasta1)
    }

    assert {
      List(emptyFasta,emptyFasta,fasta2,emptyFasta,fasta1,fasta2,fasta2,fasta1,emptyFasta).flatMap(_.lines).iterator.buffered.parseFasta.toList ==
        List(emptyFasta,emptyFasta,fasta2,emptyFasta,fasta1,fasta2,fasta2,fasta1,emptyFasta)
    }
  }
}
