import compat.Platform
import java.io.{FileOutputStream, OutputStreamWriter}
import java.lang.Exception
import java.net.URI
import java.util.Random
import xml.{Elem, XML}

case class Author(id: Long, name: String, organization: Option[String], interests: Seq[String], homepage: Option[String], coauthors: Set[Author])

object AcademicData {
  val AuthorRegex = """(?s)<a href=['"](http://academic\.research\.microsoft\.com/Author/)(\d+?)['"]>(.+?)</a>""".r
  val random = new Random(Platform.currentTime)
  val userAgents = Array(
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.21 (KHTML, like Gecko) Chrome/19.0.1042.0 Safari/535.21",
    "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.21 (KHTML, like Gecko) Chrome/19.0.1041.0 Safari/535.21",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_3) AppleWebKit/535.20 (KHTML, like Gecko) Chrome/19.0.1036.7 Safari/535.20",
    "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/535.2 (KHTML, like Gecko) Chrome/18.6.872.0 Safari/535.2 UNTRUSTED/1.0 3gpp-gba UNTRUSTED/1.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/535.19 (KHTML, like Gecko) Chrome/18.0.1025.11 Safari/535.19",
    "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.56 Safari/535.11",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.56 Safari/535.11",
    "Mozilla/5.0 (Windows NT 6.0; WOW64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.56 Safari/535.11",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.12 Safari/535.11",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.8 (KHTML, like Gecko) Chrome/17.0.940.0 Safari/535.8",
    "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.77 Safari/535.7ad-imcjapan-syosyaman-xkgi3lqg03!wgz",
    "Mozilla/5.0 (X11; CrOS i686 1193.158.0) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.75 Safari/535.7",
    "Mozilla/5.0 (Windows NT 6.0; WOW64) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.75 Safari/535.7",
    "Mozilla/5.0 (Windows NT 6.0) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.75 Safari/535.7",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.63 Safari/535.7xs5D9rRDFpg2g",
    "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/535.8 (KHTML, like Gecko) Chrome/16.0.912.63 Safari/535.8",
    "Mozilla/5.0 (Windows NT 5.2; WOW64) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.63 Safari/535.7",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.36 Safari/535.7",
    "Mozilla/5.0 (Windows NT 6.0; WOW64) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.36 Safari/535.7",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.36 Safari/535.7",
    "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.6 (KHTML, like Gecko) Chrome/16.0.897.0 Safari/535.6",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/535.2 (KHTML, like Gecko) Chrome/15.0.874.54 Safari/535.2",
    "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.2 (KHTML, like Gecko) Ubuntu/11.10 Chromium/15.0.874.120 Chrome/15.0.874.120 Safari/535.2",
    "Mozilla/5.0 (Windows NT 6.0) AppleWebKit/535.2 (KHTML, like Gecko) Chrome/15.0.874.120 Safari/535.2",
    "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.2 (KHTML, like Gecko) Chrome/15.0.872.0 Safari/535.2",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.2 (KHTML, like Gecko) Ubuntu/11.04 Chromium/15.0.871.0 Chrome/15.0.871.0 Safari/535.2",
    "Mozilla/6.0 (Macintosh; I; Intel Mac OS X 11_7_9; de-LI; rv:1.9b4) Gecko/2012010317 Firefox/10.0a4",
    "Mozilla/5.0 (Macintosh; I; Intel Mac OS X 11_7_9; de-LI; rv:1.9b4) Gecko/2012010317 Firefox/10.0a4",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.6; rv:9.0a2) Gecko/20111101 Firefox/9.0a2",
    "Mozilla/5.0 (Windows NT 6.2; rv:9.0.1) Gecko/20100101 Firefox/9.0.1",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.6; rv:9.0) Gecko/20100101 Firefox/9.0",
    "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:6.0a2) Gecko/20110613 Firefox/6.0a2",
    "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:6.0a2) Gecko/20110612 Firefox/6.0a2",
    "Mozilla/5.0 (X11; Linux i686; rv:6.0) Gecko/20100101 Firefox/6.0",
    "Mozilla/5.0 (Windows NT 6.1; rv:6.0) Gecko/20110814 Firefox/6.0",
    "Mozilla/5.0 (Windows NT 5.1; rv:6.0) Gecko/20100101 Firefox/6.0 FirePHP/0.6",
    "Mozilla/5.0 (Windows NT 5.0; WOW64; rv:6.0) Gecko/20100101 Firefox/6.0",
    "Mozilla/5.0 (X11; Linux i686 on x86_64; rv:5.0a2) Gecko/20110524 Firefox/5.0a2",
    "Mozilla/5.0 (Windows NT 6.1; U; ru; rv:5.0.1.6) Gecko/20110501 Firefox/5.0.1 Firefox/5.0.1",
    "mozilla/3.0 (Windows NT 6.1; rv:2.0.1) Gecko/20100101 Firefox/5.0.1",
    "Mozilla/5.0 (X11; U; Linux i586; de; rv:5.0) Gecko/20100101 Firefox/5.0",
    "Mozilla/5.0 (X11; U; Linux amd64; rv:5.0) Gecko/20100101 Firefox/5.0 (Debian)",
    "Mozilla/5.0 (X11; U; Linux amd64; en-US; rv:5.0) Gecko/20110619 Firefox/5.0",
    "Mozilla/5.0 (X11; Linux) Gecko Firefox/5.0",
    "Mozilla/5.0 (X11; Linux x86_64; rv:5.0) Gecko/20100101 Firefox/5.0 FirePHP/0.5",
    "Mozilla/5.0 (X11; Linux x86_64; rv:5.0) Gecko/20100101 Firefox/5.0 Firefox/5.0",
    "Mozilla/5.0 (X11; Linux x86_64) Gecko Firefox/5.0",
    "Mozilla/5.0 (X11; Linux ppc; rv:5.0) Gecko/20100101 Firefox/5.0",
    "Mozilla/5.0 (X11; Linux AMD64) Gecko Firefox/5.0",
    "Mozilla/5.0 (X11; FreeBSD amd64; rv:5.0) Gecko/20100101 Firefox/5.0",
    "Mozilla/5.0 (Windows NT 6.2; WOW64; rv:5.0) Gecko/20100101 Firefox/5.0",
    "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:5.0) Gecko/20110619 Firefox/5.0",
    "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:5.0) Gecko/20100101 Firefox/5.0",
    "Mozilla/5.0 (Windows NT 6.1.1; rv:5.0) Gecko/20100101 Firefox/5.0",
    "Mozilla/5.0 (Windows NT 5.2; WOW64; rv:5.0) Gecko/20100101 Firefox/5.0",
    "Mozilla/5.0 (Windows NT 5.1; U; rv:5.0) Gecko/20100101 Firefox/5.0",
    "Mozilla/5.0 (Windows NT 5.1; rv:2.0.1) Gecko/20100101 Firefox/5.0",
    "Mozilla/5.0 (Windows NT 5.0; WOW64; rv:5.0) Gecko/20100101 Firefox/5.0",
    "Mozilla/5.0 (Windows NT 5.0; rv:5.0) Gecko/20100101 Firefox/5.0",
    "Mozilla/5.0 (U; Windows NT 5.1; rv:5.0) Gecko/20100101 Firefox/5.0",
    "Mozilla/5.0 (Windows; U; MSIE 9.0; WIndows NT 9.0; en-US))",
    "Mozilla/5.0 (Windows; U; MSIE 9.0; Windows NT 9.0; en-US)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 7.1; Trident/5.0)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0; SLCC2; Media Center PC 6.0; InfoPath.3; MS-RTC LM 8; Zune 4.7)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0; SLCC2; Media Center PC 6.0; InfoPath.3; MS-RTC LM 8; Zune 4.7",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0; SLCC2; .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; Zune 4.0; InfoPath.3; MS-RTC LM 8; .NET4.0C; .NET4.0E)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0; chromeframe/12.0.742.112)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0; .NET CLR 3.5.30729; .NET CLR 3.0.30729; .NET CLR 2.0.50727; Media Center PC 6.0)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Win64; x64; Trident/5.0; .NET CLR 3.5.30729; .NET CLR 3.0.30729; .NET CLR 2.0.50727; Media Center PC 6.0)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Win64; x64; Trident/5.0; .NET CLR 2.0.50727; SLCC2; .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; Zune 4.0; Tablet PC 2.0; InfoPath.3; .NET4.0C; .NET4.0E)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Win64; x64; Trident/5.0",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0; yie8)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0; SLCC2; .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; InfoPath.2; .NET CLR 1.1.4322; .NET4.0C; Tablet PC 2.0)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0; FunWebProducts)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0; chromeframe/13.0.782.215)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0; chromeframe/11.0.696.57)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0) chromeframe/10.0.648.205",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.0; Trident/5.0; chromeframe/11.0.696.57)",
    "Mozilla/5.0 ( ; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0)",
    "Mozilla/4.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/4.0; FDM; MSIECrawler; Media Center PC 5.0)"
  )
  var authorsIds = Set.empty[Long]


  def getXML(host: String, path: String, query: String) = {
    val requestUrl = new URI("http", host, path, query, null).toURL
    println("fetching: " + requestUrl)

    var trickCount = 0
    def trick(): Elem = {
      trickCount += 1
      if (trickCount > 7) throw new Exception("Can't fetch URL: " + requestUrl)

      val timeout = math.abs(random.nextInt()) % 1000
      println("--> Sleep for " + timeout + "ms. Trick count: " + trickCount)
      Thread.sleep(timeout)

      val conn = requestUrl.openConnection
      conn.setConnectTimeout(15 * 1000)
      val userAgent = userAgents(random.nextInt(userAgents.length))
      conn.setRequestProperty("User-Agent", userAgent)
      try {
        XML.load(conn.getInputStream)
      } catch {
        case ex: Exception =>
          println("NOTE: user-agent " + userAgent + " is banned. I try another one")
          Thread.sleep(10 * 1000)
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

    print((if (fetchCoauthors) "Author " else "Coauthor "))
    val xml = getXML("academic.research.microsoft.com", "/Rss/" + id, null)

    val Organization = """(?s)<p>([\w\d\s]+?)<br/></p>""".r
    val InterestsAndHomepage = """(?s)<a href="http\://[a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,3}/\S*?">(?!http://academic\.research\.microsoft\.com)(.+?)</a>""".r

    val items = xml \\ "item"
    if (items.length == 0) throw new Exception("No information for " + id)

    val authorInfo = items(0)

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
        else transform(it, interests :+ buffer.replace("&#38;", "&"), interestOrHomepage)
      } else {
        if (buffer.startsWith("""http://""")) (interests, Some(buffer))
        else (interests :+ buffer.replace("&#38;", "&"), None)
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
        var processed = 0
        val restCoauthsIds = coauthorsIds -- authorsIds
        val coauths = restCoauthsIds.foldRight(Set.empty[Author])((id, cauths) =>
          try {
            processed += 1
            print("(" + processed + " of " + restCoauthsIds.size + ") ")
            val coauth = fetchAuthor(id, false)
            cauths + coauth
          } catch {
            case ex: Exception =>
              ex.printStackTrace()
              cauths
          })
        authorsIds = authorsIds ++ coauthorsIds
        coauths
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

  def appendData(author: Author, priority: Int) = {
    val (org, country, isOur) = author.organization match {
      case Some(x) => Bing.getData(x) match {
        case Some(i) => (x, i.countryRegion, (if (i.isEME) "YES" else "NO"))
        case None => (x, "Not found on the map", "???")
      }
      case None => ("Unavailable on MS site", "", "NO")
    }
    val hp = author.homepage match {
      case Some(x) => x
      case None => ""
    }
    val interests = author.interests.mkString(" - ")
    val output = "%d;%s;%d;%s;%s;%s;%s;%s".format(author.id, author.name, priority, org, country, hp, interests, isOur)

    val fileWriter = new OutputStreamWriter(new FileOutputStream("report.csv", true), "UTF-8")
    fileWriter.write(output.toString + "\n")
    fileWriter.close
  }

  def getData(query: String) = {
    val xml = getXML("academic.research.microsoft.com", "/Rss/", "query=" + query + "&searchtype=0")
    val publications = xml \\ "item"
    val authsIds =
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

    authorsIds ++= authsIds

    // TODO: MOVE TO FORMATTER
    var processed = 0
    authsIds.foreach(id => {
      try {
        val author = fetchAuthor(id, true)
        appendData(author, 1)

        var coauthsProcessed = 0
        author.coauthors.foreach(auth => {
          coauthsProcessed += 1
          println("Coauthors processed: " + coauthsProcessed + " of " + author.coauthors.size)
          appendData(auth, 2)
        })

        processed += 1
        println("--> Data added. " + processed + " of " + authsIds.size + " are processed")
      } catch {
        case ex: Exception => ex.printStackTrace()
      }
    })

    //println(authorsIds)
  }
}