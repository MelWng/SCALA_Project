package fileReader.service
import java.io.File
import scala.io.Source

final case class ReadResult[A](lines: Iterator[A], nbInvalidLine: Int)

object Parsing {


  def read[A](filePath: String, parseLine: Array[String] => Option[A]): ReadResult[A] = {

    val file = new File(filePath)
    val lines = Source.fromFile(file)
      .getLines()
      .drop(1)
      .map(line => line.split(','))
      .map(parseLine)

    val (parsedLine,invalidLines)= lines.partition(line => line.isDefined)
    ReadResult(parsedLine.flatten, invalidLines.size)

  }
}