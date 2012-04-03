package fetcher

import io.Source
import java.io.{File, FileWriter}

object Main {
  def bingDataTest() = {
    val place = "Moscow State University"
    //val place = "University of Southampton"
    Bing.getData(place) match {
      case Some(x) => println(x.countryRegion + " | " + x.isEME)
      case None => println("NO DATA")
    }
  }

  def academDataTest() = {
    AcademicData.getData("gromacs year>=2007")
  }

  def main(args: Array[String]): Unit = {
    //println(countries)
    //bingDataTest()
    //academDataTest()

    new File("dumps").mkdir
    val query = Source.fromFile("query.txt").getLines.next
    println("Query: " + query)
    AcademicData.getData(query)
  }
}