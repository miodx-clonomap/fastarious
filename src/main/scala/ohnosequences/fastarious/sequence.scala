package ohnosequences.fastarious

/*
  ## Sequence
*/
case class Sequence(val letters: String) extends AnyVal {

  def isEmpty: Boolean =
    letters.isEmpty

  def length: Int =
    letters.length

  def at(index: Int): Option[Symbol] =
    letters.lift(index)

  def headOption: Option[Symbol] =
    letters.headOption

  def tailOption: Option[Sequence] =
    if(isEmpty) None else Some { drop(1) }

  def drop(n: Int): Sequence =
    Sequence( letters drop n )

  def dropRight(n: Int): Sequence =
    Sequence( letters dropRight n )

  def slice(from: Int, until: Int): Sequence =
    Sequence( letters.slice(from, until) )

  def take(n: Int): Sequence =
    Sequence( letters take n )

  def takeRight(n: Int): Sequence =
    Sequence( letters takeRight n )

  def :+(s: Symbol): Sequence =
    Sequence(letters :+ s)

  def +:(s: Symbol): Sequence =
    Sequence(s +: letters)

  def ++(other: Sequence): Sequence =
    Sequence(letters ++ other.letters )

  def foldLeft[X](init: X)(op: (X,Symbol) => X): X =
    letters.foldLeft(init)(op)

  def reverse: Sequence =
    Sequence(letters.reverse)

  def asLinesFASTA: Seq[String] =
    letters.grouped(70).toSeq

  def asStringFASTA: String =
    asLinesFASTA.mkString("\n")
}

case object Sequence {

  val empty: Sequence =
    Sequence("")
}
