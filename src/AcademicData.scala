import java.io.FileWriter
import java.lang.Exception
import java.net.URI
import java.text.SimpleDateFormat
import java.util.Date
import xml.XML

object AcademicData {
  val AuthorRegex = """(?s)<a href=['"](http://academic\.research\.microsoft\.com/Author/)(\d+?)['"]>(.+?)</a>""".r

  def fetchAuthor(id: Long) = {
    // NOTE: An author Rss at academic.research.microsoft.com doesn't contain separate fields for organization,
    // interests and homepage. All these fields are in CDATA. Additional RegEx parsing is required.

    val requestUrl = new URI("http", "academic.research.microsoft.com", "/Rss/" + id, null).toURL
    val conn = requestUrl.openConnection
    val xml = XML.load(conn.getInputStream)

    val Organization = """(?s)<p>([\w\d\s]+?)<br/></p>""".r
    val InterestsAndHomepage = """(?s)<a href="http\://[a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,3}/\S*?">(?!http://academic\.research\.microsoft\.com)(.+?)</a>""".r

    val authorDescription = (xml \\ "item")(0) \\ "description" // NOTE: First item is an author info. Rest items are author's publications.
    val organization = (Organization findFirstIn authorDescription.text) match {
      case Some(Organization(x)) => Some(x)
      case None => None
    }

    def transform(it: Iterator[String], interests: Seq[String], buffer: String): (Seq[String], Option[String]) = {
      // Returns: list of interests and homepage if the author has it.
      if (it.hasNext) {
        val InterestsAndHomepage(interestOrHomepage) = it.next
        if (buffer.equals("")) transform(it, interests, interestOrHomepage)
        else transform(it, interests :+ buffer, interestOrHomepage)
      } else {
        if (buffer.startsWith("""http://""")) (interests, Some(buffer))
        else (interests :+ buffer, None)
      }
    }

    val (interests, homepage) = transform(InterestsAndHomepage findAllIn authorDescription.text, Seq.empty[String], "")

    val publications = (xml \\ "item").tail // NOTE: First item is an author info. Rest items are author's publications.
    val coauthorsIds: Set[Long] = publications.foldRight(Set.empty[Long])((publication, ids) => {
      val publicationDescription = publication \\ "description"
      val publicationAuthors = AuthorRegex findAllIn publicationDescription.text
      val authorsIds = publicationAuthors.map(_ match {
        case AuthorRegex(url, id, fullname) => id.toLong
        case _ => throw new Exception("No author of publication?! " + publication.text)
      })
      ids ++ authorsIds
    })


    println(organization)
    println(coauthorsIds)
    println(interests.mkString("[Array:", " | ", "]"))
    println(homepage)

    /*
    val pp = new scala.xml.PrettyPrinter(80, 4)
    val builder = new StringBuilder()
    pp.format(xml, builder)
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss")
    val date = new Date
    val fileWriter = new FileWriter("test-%s.xml".format(dateFormat.format(date)))
    fileWriter.write(builder.toString)
    fileWriter.close()

    println(((xml \\ "item")(0) \\ "description").text)
    */

    //XML.save("test.xml", xml, "UTF8", true)
  }

  def getData(query: String) = {
    val requestUrl = new URI("http", "academic.research.microsoft.com", "/Rss", "query=" + query + "&searchtype=0", null).toURL
    val conn = requestUrl.openConnection
    val xml = XML.load(conn.getInputStream)
    val publications = xml \\ "item"
    val authorsIds: Set[Long] =
      publications.foldLeft(Set.empty[Long])(
        (ids, publication) => {
          val description = publication \\ "description"
          val publicationAuthors = AuthorRegex findAllIn description.text
          val authorsIds = publicationAuthors.map(_ match {
            case AuthorRegex(url, id, fullname) => id.toLong
            case _ => throw new Exception("No author of publication?! " + publication.text)
          })
          ids ++ authorsIds
        }
      )
    println(authorsIds)
    fetchAuthor(20757032)
    //fetchAuthor(12737437)
    //fetchAuthor(55621649)
    //println(publications.length)
  }

}