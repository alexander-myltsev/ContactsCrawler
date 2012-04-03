package fetcher

import io.Source
import java.io.FileWriter
import java.net.URI
import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.mutable.StringBuilder
import xml.{Node, XML}

case class BingResponse(statusCode: Int, statusDescription: String, adminDistrict: String, countryRegion: String, /*locality: String,*/ isEME: Boolean) {
  override def toString =
    "[BingResponse: statusCode = %d, statusDescription = %s, adminDistrict = %s, countryRegion = %s]".format(
      statusCode, statusDescription, adminDistrict, countryRegion)
}

object Bing {
  val countries = {
    val lines = for (line <- Source.fromFile("countries.txt").getLines)
    yield (line.split(" - ")(1))
    lines.toSet
  }

  def dump(place: String, msg: String, url: String, node: xml.Node) = {
    //XML.save("dump.xml", node, "UTF8", true)
    val pp = new xml.PrettyPrinter(80, 4)
    val builder = new StringBuilder()
    pp.format(node, builder)
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss-SSS")
    val date = new Date
    val fileWriter = new FileWriter("dumps/dump-%s.xml".format(dateFormat.format(date)))
    fileWriter.write("Msg: " + msg + "\n")
    fileWriter.write("Place: " + place + "\n")
    fileWriter.write("URL: " + url + "\n\n")
    fileWriter.write(builder.toString)
    fileWriter.close()
  }

  def responseFromXML(node: xml.Node, countries: Set[String]) = {
    if ((node \\ "ResourceSet").length > 1) {
      throw new Exception("ResourceSets.length > 1")
    }
    val resourceSet = node \\ "ResourceSet"
    val statusCode = (node \\ "StatusCode").text.toInt
    val statusDescription = (node \\ "StatusDescription").text
    val estimatedTotal = (resourceSet \\ "EstimatedTotal").text.toInt

    if (estimatedTotal == 0) {
      throw new Exception("EstimatedTotal == 0")
    } else if (estimatedTotal == 1) {
      val country = (resourceSet \\ "CountryRegion").text
      new BingResponse(statusCode, statusDescription, (resourceSet \\ "AdminDistrict").text,
        country, countries.contains(country))
    } else if (estimatedTotal > 1) {
      val locations = resourceSet \\ "Location"
      val locOpt = locations.find(l => countries.contains((l \\ "CountryRegion").text))
      locOpt match {
        case None => throw new Exception("Countries don't contain location")
        case Some(x) =>
          new BingResponse(statusCode, statusDescription, (x \\ "AdminDistrict").text,
            (x \\ "CountryRegion").text, true)
      }
    } else {
      throw new Exception("Unexpected EstimatedTotal: " + estimatedTotal)
    }
  }

  def getData(place: String) = {
    val bingKey = """AkwBTtVf8sI-lfyIFfZ-7FSQoxKT_qwkM9xPmCrEq73FUrEOQB2tK-4RUNknDPUj"""
    // NOTE: http://msdn.microsoft.com/en-us/library/ff701714.aspx - BingMap REST API
    val xml =
      try {
        XmlDownloader.getXML("dev.virtualearth.net", "/REST/v1/Locations/-/-/-/" + place, "o=xml&key=" + bingKey)
      } catch {
        case ex: Exception =>
          ex.printStackTrace()
          null
      }
    if (xml == null) None
    else
      try {
        val response = Bing.responseFromXML(xml, countries)
        Some(response)
      } catch {
        case ex: Exception =>
          ex.printStackTrace()
          val url = new URI("http", "dev.virtualearth.net", "/REST/v1/Locations/-/-/-/" + place, "o=xml&key=" + bingKey, null).toURL
          dump(place, ex.getMessage, url.toString, xml)
          None
      }
  }
}