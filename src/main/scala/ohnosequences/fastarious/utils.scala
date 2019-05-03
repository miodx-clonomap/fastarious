package ohnosequences.fastarious

case object utils {

  def removeAllSpace(s: String): String =
    (s split('\n') map { _.trim.filter(_ >= ' ') } mkString)
}
