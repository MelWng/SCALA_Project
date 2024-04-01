package fileReader.model

import scala.util.Try

final case class Airports(id : Long, ident: String, tipe : String, name: String, continent: String, isoCountry: String,isoRegion: String)

final class NonNumber private(isoCountry : String)
object NonNumber{

  def apply(isoCountry: String): Option[String] = if (!isoCountry.forall(Character.isDigit)) {
    Some(isoCountry)
  } else None
}

//latitude_deg: String,longitude_deg: String, elevation_ft: String,municipality: String,scheduled_service: String, gps_code: String,iata_code: String,local_code: String,home_link: String,wikipedia_link: String,keywords:String)

object Airports {
  def fromCsvLine(line: Array[String]): Option[Airports] = {
    (Try(line(0).toLong).toOption, Try(line(1)).toOption,Try(line(2)).toOption,Try(line(3)).toOption,Try(line(7)).toOption, NonNumber(line(8)),Try(line(9)).toOption) match {
      case (Some(id), Some(ident),Some(tipe),Some(name),Some(continent),Some(isoCountry),Some(isoRegion)) => Some(Airports(id, ident,tipe,name,continent,isoCountry ,isoRegion))
      case other => None
    }

  }
}
