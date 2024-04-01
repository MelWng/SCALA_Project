package fileReader.service
import fileReader.model.Runways
import scala.io.StdIn.readLine

object UserInterface {
  def menu(): Unit = {
    print("Enter 1 for Query or 2 for Reports : ")
    val choice = readLine()

    choice match {
      case "1" => query()
      case "2" => reports()
    }
  }

  def query(): Unit = {
    print("Enter 1 for Country Code or 2 for Country Name : ")
    val queryCountry = readLine()

    queryCountry match {
      case "1" => queryByCountryCode()
      case "2" => queryByCountryName()

    }
  }

  def queryByCountryCode(): Unit = {
    print("Enter a Country Code : ")
    val countryCode = readLine()
    val queryCode = StoringQuering.queryCode(countryCode)
    printListRunawaysByAirportByCountry(queryCode)
  }

  def queryByCountryName(): Unit = {
    print("Enter a Country Name : ")
    val countryName = readLine()
    val queryName = StoringQuering.queryName(countryName)
    printListRunawaysByAirportByCountry(queryName)
  }

  def printListRunawaysByAirportByCountry(listRunawaysByAirport: (List[String], List[(String, List[Runways])])): Unit = {
    print("Here are the search associated with your search : ")
    listRunawaysByAirport._1.foreach(print)
    listRunawaysByAirport._2.foreach(x => println("\nAiport Name " + x._1)+ "" + x._2
      .foreach(r => println("- Runways:" + " leIdent:" + r.leIdent + " Surface:" + r.surface + " airportIdent:" + r.airportIdent + " Id:" + r.id + " Closed:" + r.closed)))

  }

  def printTypeRunwaysPerCountry(): Unit = {
    val mapTypeRunwaysPerCountry = StoringQuering.typeRunwaysPerCountry()
    mapTypeRunwaysPerCountry
      .foreach(x => print("\nCountry " + x._1 + "\ntype of Runaway :") + "" + x._2.map(e => print(e + " ")) + print("\n"))
  }

    def printReportCountries(): Unit = {
    val tenHighestCountries= StoringQuering.reportTenHighestAndLowestAirportsCountries()._1
    val tenLowestCountries= StoringQuering.reportTenHighestAndLowestAirportsCountries()._2
    println("\nThe 10 countries with highest number of airports : ")
    tenHighestCountries.foreach(x=> println(" Country "+ x._1 + " with " + x._2 + " instances"))
    println("\nThe 10 countries with lowest number of airports : ")
    tenLowestCountries.foreach(x=> println(" Country "+ x._1 + " with " + x._2 + " instances"))
  }
  def reports(): Unit={
    print("Enter : \n" +
      "1- 10 countries with highest number of airports (with count) and countries  with lowest number of airports. \n" +
      "2- Type of runways per country \n" +
      "3- The top 10 most common runway latitude \n")

    val country = readLine()
    country match {
      case "1" => printReportCountries()
      case "2" => printTypeRunwaysPerCountry()
      case "3" => printTopRunawayLatitude(StoringQuering.commonRunwayLatitude())
    }

    def printTopRunawayLatitude(listTenRunawayLatitude :List[(String, Int)]): Unit = {
      listTenRunawayLatitude.foreach(x => println("\nLatitude named " + x._1 + " has " +x._2 +" instances"))
    }  }
}
