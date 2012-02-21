import compat.Platform
import java.io.FileWriter
import java.lang.Exception
import java.net.URI
import java.text.SimpleDateFormat
import java.util.{Random, Date}
import xml.{Elem, XML}

case class Author(id: Long, name: String, organization: Option[String], interests: Seq[String], homepage: Option[String], coauthors: Set[Author])

object AcademicData {
  val AuthorRegex = """(?s)<a href=['"](http://academic\.research\.microsoft\.com/Author/)(\d+?)['"]>(.+?)</a>""".r
  val random = new Random(Platform.currentTime)
  var version = 5

  def getXML(host: String, path: String, query: String) = {
    val requestUrl = new URI("http", host, path, query, null).toURL
    println("fetching: " + requestUrl)

    def trick(): Elem = {
      val timeout = math.abs(random.nextInt()) % 3000
      println("--> Sleep for " + timeout + "ms")
      Thread.sleep(timeout)

      version = (version + 1) % 100
      val conn = requestUrl.openConnection
      val userAgent = "Mozilla/" + version + ".0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.2.3) Gecko/20100401"
      conn.setRequestProperty("User-Agent", userAgent)
      try {
        XML.load(conn.getInputStream)
      } catch {
        case ex: Exception =>
          println("NOTE: user-agent" + userAgent + " is banned. I try another one.")
          trick()
      }
    }

    val xml = trick()
    //println(xml.toString)
    xml
  }

  def fetchAuthor(id: Long, fetchCoauthors: Boolean): Author = {
    // NOTE: An author Rss at academic.research.microsoft.com doesn't contain separate fields for organization,
    // interests and homepage. All these fields are in CDATA. Additional RegEx parsing is required.

    val xml = getXML("academic.research.microsoft.com", "/Rss/" + id, null)

    val Organization = """(?s)<p>([\w\d\s]+?)<br/></p>""".r
    val InterestsAndHomepage = """(?s)<a href="http\://[a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,3}/\S*?">(?!http://academic\.research\.microsoft\.com)(.+?)</a>""".r

    val authorInfo = (xml \\ "item")(0)

    val authorName = {
      val s = (authorInfo \\ "title").text
      s.substring(0, s.indexOf(" (Personal Info)"))
    }

    val authorDescription = authorInfo \\ "description" // NOTE: First item is an author info. Rest items are author's publications.
    val authorOrganization = (Organization findFirstIn authorDescription.text) match {
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

    val coauthors: Set[Author] =
      if (fetchCoauthors) {
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
        coauthorsIds.map(id => fetchAuthor(id, false))
      } else Set.empty[Author]

    /*
    println(organization)
    println(coauthorsIds)
    println(interests.mkString("[Array:", " | ", "]"))
    println(homepage)
    */

    Author(id, authorName, authorOrganization, interests, homepage, coauthors)

    /*
    val pp = new scala.xml.PrettyPrinter(80, 4)
    val builder = new StringBuilder()
    pp.format(xml, builder)
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH-mm-ss")
    val date = new Date
    val fileWriter = new FileWriter("test-%s.xml".format(dateFormat.format(date)))
    fileWriter.write(builder.toString)
    fileWriter.close

    println(((xml \\ "item")(0) \\ "description").text)
    */

    //XML.save("test.xml", xml, "UTF8", true)
  }

  def getData(query: String) = {
    val xml = getXML("academic.research.microsoft.com", "/Rss/", "query=" + query + "&searchtype=0")
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


    authorsIds.foreach(id => {
      val author = fetchAuthor(id, false)
      /*
      val output =
        <p>
          {author.id + " " + author.name}<ul>
          {for (coauthor <- author.coauthors) yield {

            <li>
              {coauthor.id + " " + coauthor.name}
            </li>
          }}
        </ul>
        </p>
        */
      val output =
        <p>
          <a href={"http://academic.research.microsoft.com/Author/" + author.id}>
            go-to-ms
          </a>
          <a href={author.homepage match {
            case Some(x) => x
            case None => "no_homepage"
          }}>
            {author.name}
          </a>{"[Location: " + (author.organization match {
          case Some(x) => x + ", " + (Bing.getData(x) match {
            case Some(r) => r.countryRegion
            case None => "NOT FOUND"
          })
          case None => "NONE"
        }) + "] " + "[Interests: " + author.interests.mkString(" | ") + "]"}
        </p>
      println(output.toString)
      val fileWriter = new FileWriter("report.html", true)
      fileWriter.write(output.toString + "\n")
      fileWriter.close
    })
    //println(authorsIds)
    //fetchAuthor(20757032, true)
    //fetchAuthor(12737437)
    //fetchAuthor(55621649)
    //println(publications.length)
  }

}