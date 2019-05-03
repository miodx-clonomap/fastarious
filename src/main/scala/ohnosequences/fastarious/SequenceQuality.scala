package ohnosequences.fastarious

/*
  ## SequenceQuality

  A fastq sequence has the sequence itself plus the corresponding quality. At construction, a `SequenceQuality` is checked to have the same length for sequence and quality.
*/
// TODO re-evaluate this design. Probably better to have Seq[(Char, Qual)] as primitive
// NOTE the *constructor* is private here, not the values.
case class SequenceQuality private[fastarious] (val sequence: Sequence, val quality: Quality) {

  def isEmpty: Boolean =
    sequence.isEmpty

  def length: Int =
    sequence.length

  def at(index: Int): Option[QSymbol] = for {
    s <- sequence.at(index)
    q <- quality.at(index)
  } yield QSymbol(s, q)

  def headOption: Option[QSymbol] = for {
    s <- sequence.headOption
    q <- quality.headOption
  } yield QSymbol(s, q)

  def tailOption: Option[SequenceQuality] =
    if(isEmpty) None else Some { drop(1) }

  def drop(n: Int): SequenceQuality =
    SequenceQuality( sequence drop n, quality drop n )

  def dropRight(n: Int): SequenceQuality =
    SequenceQuality( sequence dropRight n, quality dropRight n )

  def slice(from: Int, until: Int): SequenceQuality =
    SequenceQuality( sequence.slice(from, until), quality.slice(from, until) )

  def take(n: Int): SequenceQuality =
    SequenceQuality( sequence take n, quality take n )

  def takeRight(n: Int): SequenceQuality =
    SequenceQuality( sequence takeRight n, quality takeRight n )

  def :+(qs: QSymbol): SequenceQuality =
    SequenceQuality(sequence :+ qs.symbol, quality :+ qs.score)

  def +:(qs: QSymbol): SequenceQuality =
    SequenceQuality(qs.symbol +: sequence, qs.score +: quality)

  def ++(other: SequenceQuality): SequenceQuality =
    SequenceQuality( sequence ++ other.sequence, quality ++ other.quality )

  def foldLeft[X](init: X)(op: (X,QSymbol) => X): X =
    qSymbols.foldLeft(init)(op)

  def reverse: SequenceQuality =
    SequenceQuality( sequence.reverse, quality.reverse )

  def qSymbols: Seq[QSymbol] =
    (sequence.letters zip quality.scores) map { case (sy,sc) => QSymbol(sy,sc) }

  def pSymbols: Seq[PSymbol] =
    (sequence.letters zip quality.scores) map { case (sy,sc) => PSymbol(sy,Quality.errorProbability(sc)) }

  def asStringPhred33: String = Seq(
    sequence.letters,
    "+",
    quality.toPhred33
  ).mkString("\n")

  /*
    ### Filtering operations

    All these methods (filter, takeWhile/dropWhile, etc) take a predicate on *both* letters and quality. There are convenience methods `xyzSequence` and `xyzQuality` which have the same behavior as `xyz` but for predicates on letters and quality respectively.
  */

  /*
    #### filter
  */
  def filter(p: QSymbol => Boolean): SequenceQuality = {
    val (seq, qual) =
      (sequence.letters zip quality.scores)
        .filter { case (c, q) => p(QSymbol(c, q)) }
        .unzip

    SequenceQuality(Sequence(seq.mkString), Quality(qual))
  }

  def filterSequence(p: Symbol => Boolean): SequenceQuality =
    filter { case QSymbol(s, _) => p(s) }

  def filterQuality(p: Score => Boolean): SequenceQuality =
    filter { case QSymbol(_, q) => p(q) }

  /*
    #### count
  */
  def count(p: QSymbol => Boolean): Int =
    (sequence.letters zip quality.scores)
      .count { case (c, q) => p(QSymbol(c, q)) }

  def countSequence(p: Symbol => Boolean): Int =
    count { case QSymbol(s, _) => p(s) }

  def countQuality(p: Score => Boolean): Int =
    count { case QSymbol(_, q) => p(q) }

  /*
    #### takeWhile
  */
  def takeWhile(p: QSymbol => Boolean): SequenceQuality = {
    val (seq, qual) =
      (sequence.letters zip quality.scores)
        .takeWhile { case (c, q) => p(QSymbol(c, q)) }
        .unzip

    SequenceQuality(Sequence(seq.mkString), Quality(qual))
  }

  def takeWhileQuality(p: Score => Boolean): SequenceQuality =
    takeWhile { case QSymbol(_, q) => p(q) }

  def takeWhileSequence(p: Symbol => Boolean): SequenceQuality =
    takeWhile { case QSymbol(s, _) => p(s) }

  /*
    #### dropWhile
  */
  def dropWhile(p: QSymbol => Boolean): SequenceQuality = {
    val (seq, qual) =
      (sequence.letters zip quality.scores)
        .dropWhile { case (c, q) => p(QSymbol(c, q)) }
        .unzip

    SequenceQuality(Sequence(seq.mkString), Quality(qual))
  }

  def dropWhileQuality(p: Score => Boolean): SequenceQuality =
    dropWhile { case QSymbol(_, q) => p(q) }

  def dropWhileSequence(p: Symbol => Boolean): SequenceQuality =
    dropWhile { case QSymbol(s, _) => p(s) }

  /*
    #### span
  */
  def span(p: QSymbol => Boolean): (SequenceQuality, SequenceQuality) = {
    val (sq1, sq2) =
      (sequence.letters zip quality.scores)
        .span { case (c, q) => p(QSymbol(c, q)) }

    val (s1, q1) = sq1.unzip
    val (s2, q2) = sq2.unzip

    (
      SequenceQuality(Sequence(s1.mkString), Quality(q1)),
      SequenceQuality(Sequence(s2.mkString), Quality(q2))
    )
  }

  def spanQuality(p: Score => Boolean): (SequenceQuality, SequenceQuality) =
    span { case QSymbol(_, q) => p(q) }

  def spanSequence(p: Symbol => Boolean): (SequenceQuality, SequenceQuality) =
    span { case QSymbol(s, _) => p(s) }
}

case object SequenceQuality {

  val empty: SequenceQuality =
    SequenceQuality(Sequence.empty, Quality.empty)

  def fromStringsPhred33(rawSeq: String, rawQual: String): Option[SequenceQuality] =
    if(rawSeq.length == rawQual.length)
      Quality.fromPhred33(rawQual).map( SequenceQuality(Sequence(rawSeq), _) )
    else
      None
}

case class QSymbol(val symbol: Symbol, val score: Score) {

  def toPSymbol: PSymbol =
    PSymbol(symbol, Quality.errorProbability(score))
}

case class PSymbol(val symbol: Symbol, val errorP: ErrorP) {

  def toQSymbol: QSymbol =
    QSymbol(symbol, Quality.scoreFrom(errorP))

  def successP: ErrorP =
    1 - errorP
}
