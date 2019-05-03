package ohnosequences.fastarious.test

import org.scalatest.FunSuite
import ohnosequences.fastarious._

import DNA._, DNAQ._

class DNAOperations extends FunSuite {

  test("complement usage") {

    assert { Sequence("ATCCG").asDNA.complement === Sequence("TAGGC") }
  }

  test("reverse complement usage") {

    assert { Sequence("AATCG").asDNA.complement.reverse === Sequence("CGATT") }
  }

  test("complement preserves case") {

    assert { Sequence("AaTtCcGGg").asDNA.complement === Sequence("TtAaGgCCc") }
  }

  test("complement does nothing to extra chars") {

    assert { Sequence("ACCTACohnonoisehereCTAGNNN").asDNA.complement === Sequence("TGGATGohnonoisehereGATCNNN") }
  }

  test("complement own inverse") {

    assert { Sequence("AaTtCcGGg").asDNA.complement.asDNA.complement === Sequence("AaTtCcGGg") }
  }

  test("complement and reverse commute") {

    assert { Sequence("AaTtCcGGg").asDNA.complement.reverse === Sequence("AaTtCcGGg").reverse.asDNA.complement }
  }
}

class DNAQOperations extends FunSuite {

  val qual =
    Quality(Seq(32,40,26,33))

  val seq =
    Sequence("ATCG")

  val seqQual =
    SequenceQuality(seq, qual)

  test("complement does nothing to quality") {

    assert { seqQual.asDNAQ.complement === SequenceQuality(seqQual.sequence.asDNA.complement, seqQual.quality) }
  }

  test("complement own inverse") {

    assert { seqQual.asDNAQ.complement.asDNAQ.complement === seqQual }
  }

  test("complement and reverse commute") {

    assert { seqQual.asDNAQ.complement.reverse === seqQual.reverse.asDNAQ.complement }
  }
}
