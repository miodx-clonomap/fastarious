package ohnosequences.fastarious

case object DNA {

  implicit final
  class DNASyntax(val seq: Sequence) {

    def asDNA: DNAops =
      DNAops(seq)
  }

  case class DNAops(val seq: Sequence) {

    def complement: Sequence =
      Sequence(seq.letters.map(complementaryChar))
  }

  def complementaryChar(c: Char): Char =
    c match {
      case 'A'  => 'T'
      case 'T'  => 'A'
      case 'C'  => 'G'
      case 'G'  => 'C'
      case 'a'  => 't'
      case 't'  => 'a'
      case 'c'  => 'g'
      case 'g'  => 'c'
      case  x   =>  x
    }
}
