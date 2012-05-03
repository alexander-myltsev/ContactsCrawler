package fetcher

import io.Source
import java.net.URI
import java.io.{InputStreamReader, BufferedReader, File, FileWriter}

object Main {
  def bingDataTest() = {
    val place = "Moscow State University"
    //val place = "University of Southampton"
    val bingResp = Bing.getData(place)
    bingResp match {
      case x if x.respStatus == BingResponseStatus.Success => println(x.countryRegion + " | " + x.isEME)
      case x if x.respStatus == BingResponseStatus.XmlDownloadFail => println("XmlDownloadFail")
    }
  }

  def academDataTest() = {
    AcademicData.getData("gromacs")
  }

  def main(args: Array[String]): Unit = {
    //val url = "http://onlinelibrary.wiley.com/advanced/search/results/reentry?scope=allContent&query=gromacs&inTheLastList=6&queryStringEntered=false&searchRowCriteria[0].fieldName=all-fields&searchRowCriteria[0].booleanConnector=and&searchRowCriteria[1].fieldName=all-fields&searchRowCriteria[1].booleanConnector=and&searchRowCriteria[2].fieldName=all-fields&searchRowCriteria[2].booleanConnector=and&start=1&resultsPerPage=10&ordering=relevancy"
    //println(countries)
    //bingDataTest()
    //academDataTest()

    new File("dumps").mkdir
    val query = Source.fromFile("query.txt").getLines.next
    println("Query: " + query)
    AcademicData.getData(query)
  }
}