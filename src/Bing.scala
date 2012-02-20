import java.io.FileWriter
import java.net.URI
import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.mutable.StringBuilder
import xml.XML

case class BingResponse(statusCode: Int, statusDescription: String, adminDistrict: String, countryRegion: String, locality: String) {
  override def toString =
    "[BingResponse: statusCode = %d, statusDescription = %s, adminDistrict = %s, countryRegion = %s, locality = %s]".format(
      statusCode, statusDescription, adminDistrict, countryRegion, locality)
}

object Bing {
  def dump(place: String, url: String, node: xml.Node) = {
    //XML.save("dump.xml", node, "UTF8", true)
    val pp = new xml.PrettyPrinter(80, 4)
    val builder = new StringBuilder()
    pp.format(node, builder)
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss")
    val date = new Date
    val fileWriter = new FileWriter("dump-%s.xml".format(dateFormat.format(date)))
    fileWriter.write("Place: " + place + "\n")
    fileWriter.write("URL: " + url + "\n\n")
    fileWriter.write(builder.toString)
    fileWriter.close()
  }

  def responseFromXML(node: xml.Node) = {
    if ((node \\ "ResourceSet").length > 1) {
      throw new Exception("ResourceSets.length > 1")
    }
    val resourceSet = node \\ "ResourceSet"
    if ((resourceSet \\ "EstimatedTotal").text.toInt != 1) {
      throw new Exception("EstimatedTotal != 1")
    }
    new BingResponse(
      (node \\ "StatusCode").text.toInt,
      (node \\ "StatusDescription").text,
      (resourceSet \\ "AdminDistrict").text,
      (resourceSet \\ "CountryRegion").text,
      (resourceSet \\ "Locality").text)
  }

  def getData(place: String) = {
    val bingKey = """AkwBTtVf8sI-lfyIFfZ-7FSQoxKT_qwkM9xPmCrEq73FUrEOQB2tK-4RUNknDPUj"""
    // NOTE: http://msdn.microsoft.com/en-us/library/ff701714.aspx - BingMap REST API
    val url = new URI("http", "dev.virtualearth.net", "/REST/v1/Locations/-/-/-/" + place, "o=xml&key=" + bingKey, null).toURL
    val conn = url.openConnection
    val xml = XML.load(conn.getInputStream)
    try {
      val response = Bing.responseFromXML(xml)
      Some(response)
    } catch {
      case ex: Exception => dump(place, url.toString, xml)
      None
    }
  }
}