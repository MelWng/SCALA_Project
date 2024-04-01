package fileReader.model
import scala.util.Try

final case class Runways (id: Long, airportRef : Long, airportIdent: String, surface:String, closed:Long, leIdent:String)
final class IsAirportIdent private(airportIdent : String)
object IsAirportIdent {
  val listAlphabet = 'A' to 'Z'

  def apply(airportIdent: String): Option[String] = {

    if (airportIdent.count(listAlphabet.contains(_)) <= 4)
      Some(airportIdent)
    else None
  }
}

final class IsSurface private(surface : String)
object IsSurface {
  val listAlphabet = 'A' to 'Z'
  def apply(surface: String): Option[String] = {
    if ((surface.count(listAlphabet.contains(_)) >=3) || (surface.toUpperCase() == "\"U\""))
      Some(surface)
    else None
  }
}

object Runways {
  def fromCsvLine(line: Array[String]): Option[Runways] = {
    (Try(line(0).toLong).toOption, Try(line(1).toLong).toOption,IsAirportIdent(line(2)),IsSurface(line(5)),Try(line(7).toLong).toOption,Try(line(8)).toOption) match {
      case (Some(id), Some(airportRef),Some(airportIdent),Some(surface),Some(closed),Some(leIdent)) => Some(Runways(id, airportRef, airportIdent, surface,closed,leIdent))
      case other=> None
    }
  }
}

