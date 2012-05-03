package fetcher

import io.Source
import java.io.FileWriter
import java.net.URI
import java.text.SimpleDateFormat
import scala.collection.mutable.StringBuilder
import java.util.Date

object BingResponseStatus extends Enumeration {
  type typ = Value

  case class XmlParseFail(msg: String) extends Val(msg)

  val XmlDownloadFail, SuccessSeveral, Success = Value
}

object BingResponse {
  def apply(respStatus: BingResponseStatus.typ): BingResponse = BingResponse(respStatus, -1, null, null, null, false) // TODO: move it to case class BingResponse
}

case class BingResponse(respStatus: BingResponseStatus.typ, statusCode: Int, statusDescription: String,
                        adminDistrict: String, countryRegion: String, /*locality: String,*/ isEME: Boolean) {
  override def toString =
    "[BingResponse: respStatus = %s, statusCode = %d, statusDescription = %s, adminDistrict = %s, countryRegion = %s]".format(
      respStatus.toString, statusCode, statusDescription, adminDistrict, countryRegion)
}

object Bing {
  private val bingKey = """AkwBTtVf8sI-lfyIFfZ-7FSQoxKT_qwkM9xPmCrEq73FUrEOQB2tK-4RUNknDPUj"""
  val countries = {
    val lines =
      for (line <- Source.fromFile("countries.txt").getLines)
        yield (line.split(" - ")(1))
    lines.toSet
  }

  def isEME(address: String): Boolean = {
    val tokens = address.split("""[.\s,;]+""")
    println("\t" + address)
    //println("\t" + tokens.mkString("{", " | ", "}"))
    //println(tokens.toSet.intersect(countries))
    val isNotEME = tokens.toSet.intersect(countries).isEmpty
    println("\tisEME: " + !isNotEME)
    !isNotEME
  }

  def dump(place: String, msg: String, url: String, node: xml.Node) = {
    //XML.save("dump.xml", node, "UTF8", true)
    val pp = new xml.PrettyPrinter(80, 4)
    val builder = new StringBuilder
    pp.format(node, builder)
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss-SSS")
    val date = new Date
    val fileWriter = new FileWriter("dumps/dump-%s.xml".format(dateFormat.format(date)))
    fileWriter.write("Msg: " + msg + "\n")
    fileWriter.write("Place: " + place + "\n")
    fileWriter.write("URL: " + url + "\n\n")
    fileWriter.write(builder.toString)
    fileWriter.close
  }

  def responseFromXML(node: xml.Node, countries: Set[String]): BingResponse = {
    if ((node \\ "ResourceSet").length > 1) {
      //throw new Exception("ResourceSets.length > 1")
      return BingResponse(BingResponseStatus.XmlParseFail("ResourceSets.length > 1"))
    }
    val resourceSet = node \\ "ResourceSet"
    val statusCode = (node \\ "StatusCode").text.toInt
    val statusDescription = (node \\ "StatusDescription").text
    val estimatedTotal = (resourceSet \\ "EstimatedTotal").text.toInt

    if (estimatedTotal == 0) {
      //throw new Exception("EstimatedTotal == 0")
      BingResponse(BingResponseStatus.XmlParseFail("EstimatedTotal == 0"))
    } else if (estimatedTotal == 1) {
      val country = (resourceSet \\ "CountryRegion").text
      new BingResponse(BingResponseStatus.Success, statusCode, statusDescription, (resourceSet \\ "AdminDistrict").text,
        country, countries.contains(country))
    } else if (estimatedTotal > 1) {
      val locations = resourceSet \\ "Location"
      val locOpt = locations.find(l => countries.contains((l \\ "CountryRegion").text))
      locOpt match {
        case None =>
          //throw new Exception("Countries don't contain location")
          BingResponse(BingResponseStatus.XmlParseFail("Countries don't contain location"))
        case Some(x) =>
          BingResponse(BingResponseStatus.SuccessSeveral, statusCode, statusDescription, (x \\ "AdminDistrict").text, (x \\ "CountryRegion").text, true)
      }
    } else {
      //throw new Exception("Unexpected EstimatedTotal: " + estimatedTotal)
      BingResponse(BingResponseStatus.XmlParseFail("Unexpected EstimatedTotal: " + estimatedTotal))
    }
  }

  def getData(place: String): BingResponse = {
    // NOTE: http://msdn.microsoft.com/en-us/library/ff701714.aspx - BingMap REST API
    val xml =
      try {
        XmlDownloader.getXML("dev.virtualearth.net", "/REST/v1/Locations/-/-/-/" + place, "o=xml&key=" + bingKey)
      } catch {
        case ex: Exception =>
          ex.printStackTrace()
          null
      }
    val response = {
      if (xml == null) BingResponse(BingResponseStatus.XmlDownloadFail)
      else Bing.responseFromXML(xml, countries)
    }
    if (response.respStatus != BingResponseStatus.Success) {
      val url = new URI("http", "dev.virtualearth.net", "/REST/v1/Locations/-/-/-/" + place, "o=xml&key=" + bingKey, null).toURL
      dump(place, response.respStatus.toString, url.toString, xml)
    }
    response
  }
}