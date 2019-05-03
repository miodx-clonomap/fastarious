package ohnosequences.fastarious

import java.io._

/*
  # FASTA

  Fasta is a not so well-defined format, consisting on

  1. a header
  2. and a sequence

  As usual, it is tied with a particular representation in the form of text files, where a (multi)set of elements consists of

  ```
  >header1
  sequence1
  >header2
  sequence2
  ```

  The header *must* be **one** line, while sequences are split into `70` characters by line (the good old fixed-width terminal times).

  I know all this is crazy, but that's the way bioinformatics is.
*/
case object fasta {

  case class Header(val value: String) extends AnyVal {

    def asString: String =
      Seq(Header.prefix, value).mkString

    // NOTE: value == s"${id} ${description}"
    final def id: String = value.takeWhile(_ != ' ')
    final def description: String = value.stripPrefix(s"${id} ")
  }

  case object Header {
    final val prefix: String = ">"

    def parseFrom(raw: String): Option[Header] = {
      if (isValid(raw))
        Some(Header(raw.stripPrefix(prefix)))
      else
        None
    }

    def isValid(str: String): Boolean =
      str.startsWith(prefix)
  }

  case class FASTA(
    val header: Header,
    val sequence: Sequence
  ) {

    def lines: Seq[String] =
      header.asString +: sequence.asLinesFASTA

    def asString: String = lines.mkString("\n")
  }



  implicit class FASTAIteratorOps(val fastas: Iterator[FASTA]) extends AnyVal {

    def appendTo(file: File): File = {

      val wr = new BufferedWriter(new FileWriter(file, true))
      fastas.foreach { fa =>
        wr.write( fa.asString )
        wr.newLine
      }
      wr.close

      file
    }
  }

  /* Note, all we only use `next`, `hasNext` and `head` method here, so the `lines` iterator can be used after calling any of these extension methods and can be considered as the parsing "remainder" (i.e. unparsed part) */
  implicit class BufferedIteratorFASTAOps(val lines: BufferedIterator[String]) extends AnyVal {

    /* Similar to `takeWhile`, but the iterator **can be reused** after */
    def cutUntil(condition: String => Boolean): Seq[String] = {

      @annotation.tailrec
      def rec(acc: collection.mutable.Buffer[String]): collection.mutable.Buffer[String] = {
        if (lines.hasNext && !condition(lines.head))
          rec(acc :+ lines.next())
        else acc
      }

      rec(collection.mutable.Buffer.empty)
    }

    /* Use this method to parse all FASTA values from the `lines` iterator (with possible errors) */
    def parseFasta: Iterator[FASTA] = new Iterator[FASTA] {

      /* If there is one more header, there is one more FASTA value (even if the sequence is empty) */
      def hasNext: Boolean = lines.hasNext && Header.isValid(lines.head)

      def next(): FASTA =
        if (!hasNext) Iterator.empty.next()
        else {
          val line: String = lines.next()
          val rest: String = cutUntil(Header.isValid).mkString
          FASTA(
            // hasNext means, that line is a valid header
            Header(line.stripPrefix(Header.prefix)),
            Sequence(rest)
          )
        }
    }

    /* This is the same as `parseFasta`, but skips anything before the first FASTA value */
    def parseFastaSkipCrap: Iterator[FASTA] = {
      cutUntil(Header.isValid)
      parseFasta
    }
  }

}
