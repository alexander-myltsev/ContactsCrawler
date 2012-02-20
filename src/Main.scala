import io.Source

object Main {
  val countries = {
    val lines = for (line <- Source.fromFile("countries.txt").getLines)
    yield (line.split(" - ")(1))
    lines.toSet
  }

  def main(args: Array[String]) = {
    println(countries)
    //val place = "Moscow State University"
    val place = "Maaaakro Royal Swedish Academy of Sciences"
    Bing.getData(place) match {
      case Some(x) => println(x.countryRegion + " | " + countries.contains(x.countryRegion))
      case None => println("NO DATA")
    }
  }
}