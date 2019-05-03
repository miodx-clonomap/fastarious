package ohnosequences.fastarious

import scala.collection.JavaConverters._
import java.nio.file.Files
import java.io.File

package object test {

  // WARNING this will leak file descriptors
  def lines(jFile: File): Iterator[String] =
    Files.lines(jFile.toPath).iterator.asScala
}
