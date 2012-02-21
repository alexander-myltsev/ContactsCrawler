import compat.Platform
import io.Source
import java.io.FileWriter
import java.util.Random

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

  def main(args: Array[String]) = {
    //println(countries)
    //bingDataTest()
    academDataTest()
  }
}