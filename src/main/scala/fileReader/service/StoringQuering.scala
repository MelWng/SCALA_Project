package fileReader.service
import fileReader.model.{Airports, Countries, Runways}

object StoringQuering {
  val airportFile: ReadResult[Airports] = Parsing.read("C:/Users/melin/Documents/M1/SCALA/ScalaProjectAirport/src/main/data/airports.csv", Airports.fromCsvLine)
  val runwayFile: ReadResult[Runways] = Parsing.read("C:/Users/melin/Documents/M1/SCALA/ScalaProjectAirport/src/main/data/runways.csv", Runways.fromCsvLine)
  val countriesFile: ReadResult[Countries] = Parsing.read("C:/Users/melin/Documents/M1/SCALA/ScalaProjectAirport/src/main/data/countries.csv", Countries.fromCsvLine)
  val listAirport = airportFile.lines.toList
  val listRunway = runwayFile.lines.toList
  val listCountry = countriesFile.lines.toList

  def queryCode(countryCode: String): (List[String], List[(String, List[Runways])]) = {
    val listCode = listCountry.filter(x => x.code.toLowerCase().containsSlice(countryCode.toLowerCase()))
    val airportResult = listAirport.filter(airport => listCode.map(_.code).contains(airport.isoCountry))
    (listCode.map(_.code),airportResult.map(e => (e.name, listRunway.filter(x => e.ident.contains(x.airportIdent)))))
  }

  def queryName(countryName: String): (List[String], List[(String, List[Runways])]) = {
    val listNameCountry = listCountry.filter(x => x.name.toLowerCase().containsSlice(countryName.toLowerCase()))
    val airportResult = listAirport.filter(x => listNameCountry.map(_.code).contains(x.isoCountry))
    (listNameCountry.map(_.name),airportResult.map(e => (e.name, listRunway.filter(x => e.ident.contains(x.airportIdent)))))
  }

  def reportTenHighestAndLowestAirportsCountries(): (List[(String, Int)], List[(String, Int)]) = {
    val listAirportByCountry = listAirport.groupBy(_.isoCountry).map(e =>
      (listCountry
        .filter(c => c.code.contains(e._1))
        .map(_.name).mkString, e._2.length)).toList.sortBy(_._2)
    val tenHighestCountries = listAirportByCountry.reverse.take(10)
    val tenLowestCountries = listAirportByCountry.take(10)

    (tenHighestCountries, tenLowestCountries)
  }

  /**
   * From Airportfile, we group by isoCountry.
   * With a map :
   * key 1 -> Transform the isoCountry by the CountryName.
   * key 2 -> For each Airport, we have a list of Runaway. And we just keep the surface column with a flatMap. And distinct them.
   * @param None
   * @return A list of different surface Runaway by Country name
   *
   */
  def typeRunwaysPerCountry(): Map[String, List[String]] = {
    listAirport
      .groupBy(_.isoCountry)
      .map(e =>
        (listCountry
          .filter(c => c.code.contains(e._1))
          .map(_.name).mkString,
          regroupSameTypes(queryCode(e._1)._2.flatMap(x => x._2.map(_.surface.toUpperCase())).distinct)))
  }

  def typeOfRunaway(): Unit = listRunway.map(_.surface.toUpperCase()).distinct.sorted.foreach(println)

  def regroupSameTypes(input: List[String]): List[String] = {
    input.map{surface => surface match {
      case "\"ALUM\"" | "\"ALUMINIUM\"" => "\"ALUMINIUM\""
      case "\"ASP\"" | "\"ASPH\"" | "\"'ASPHALT'\"" | "\"'ASPHALTH'\"" | "\"ASPHALT\"" => "\"ASPHALT\""
      case "\"BIT\"" | "\"BITUMINOUS\"" => "\"BITUMINOUS\""
      case "\"BRI\"" | "\"BRICK\"" => "\"BRICK\""
      case "\"CON\"" | "\"CONCRETE\"" | "\"CONC\"" => "\"CONCRETE\""
      case "\"CLA\"" | "\"CLAY\"" => "\"CLAY\""
      case "\"GRAV\"" | "\"GRAVEL\"" | "\"GRAVEL" => "\"GRAVEL\""
      case "\"GRE\"" => "\"GRADED OR ROLLED EARTH, GRASS ON GRADED EARTH\""
      case "\"GRS\"" => "\"GRASS OR EARTH NOT GRADED OR ROLLED\""
      case "\"PEM\"" | "PYTHON" => "\"PARTIALLY CONCRETE, ASPHALT OR BITUMEN-BOUND MACADAM\""
      case "\"SAN\"" | "\"SAND\"" => "\"SAND\""
      case "\"UNK\"" | "\"UNKNOW\"" | "\"U\"" => "\"UNKNOW\""
      case _ => surface
    }
    }
  }

  def commonRunwayLatitude(): List[(String, Int)] = listRunway
    .groupBy(_.leIdent)
    .map(e => (e._1, e._2.length))
    .toList.sortBy(_._2)
    .reverse
    .take(10)
}

