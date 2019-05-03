package ohnosequences.fastarious

import DNA._

case object DNAQ {

  implicit final
  class DNAQSyntax(val seq: SequenceQuality) {

    def asDNAQ: DNAQOps =
      DNAQOps(seq)
  }

  case class DNAQOps(val seq: SequenceQuality) {

    def complement: SequenceQuality =
      SequenceQuality( seq.sequence.asDNA.complement, seq.quality )
  }
}
