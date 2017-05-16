package ohnosequences.fastarious

import Sequence._

/*
  ## Sequence
*/
// NOTE the *constructor* is private here, not the value.
case class Sequence private[fastarious] (val letters: String) extends AnyVal {

  def isEmpty: Boolean =
    letters.isEmpty

  def length: Int =
    letters.length

  def at(index: Int): Option[Symbol] =
    if( index < 0 || (length - 1) < index) None else Some( letters(index) )

  def headOption: Option[Symbol] =
    if(isEmpty) None else Some { letters.head }

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

  def ++(other: Sequence): Sequence =
    Sequence(letters ++ other.letters )
}

case object Sequence {

  type Symbol = Char
}
