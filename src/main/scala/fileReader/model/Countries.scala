package fileReader.model

import scala.util.Try
final case class Countries(id : Long, code: String, name : String, continent : String)

final class IsContinent private(continent : String)
object IsContinent{
  val listContinent = List[String]( "\"EU\"","\"AF\"","\"NA\"","\"SA\"","\"OC\"","\"AS\"")

  def apply(continent: String): Option[String] = {
    if (listContinent.contains(continent)){
      Some(continent)
    } else None
  }
}

object Countries {
  def fromCsvLine(line: Array[String]): Option[Countries] = {
    (Try(line(0).toLong).toOption, Try(line(1)).toOption,Try(line(2)).toOption,IsContinent(line(3))) match {
      case (Some(id), Some(code),Some(name),Some(continent)) => Some(Countries(id,code,name,continent))
      case other => None
    }
  }
}