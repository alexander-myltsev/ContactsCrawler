package fetcher

import java.io.{FileOutputStream, OutputStreamWriter}
import java.lang.Exception
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import java.net.{URL, URI, SocketTimeoutException}

case class Contact(correspondence: Some[String], email: Option[String])

object AcademicData {
  def fetchAuthor(index: Int, articleURL: String): Unit = {

    println("Fetching author from " + articleURL)

    val articleHtml = {
      try {
        Jsoup.connect(articleURL).timeout(120000).get
      } catch {
        case (e: SocketTimeoutException) =>
          val fileWriter = new OutputStreamWriter(new FileOutputStream("debug.txt", true), "UTF-8")
          fileWriter.write("----------------------------------------------------------\n")
          fileWriter.write(index + ": " + articleURL + "\n")
          fileWriter.write("NO RESPONSE\n")
          fileWriter.close
          return
      }
    }


    // TODO: реализовать сопоставление организаций каждому человеку. Фильтровать по стране организации. Сделать поиск домашней страницы
    val affiliations = articleHtml
      .select("li.affiliation")
      .toArray(Array[Element]())
      .map(organizationElem => {
      val id = organizationElem.select("sup").text
      val name = organizationElem.children.select("p").text
      (id, name)
    })

    val correspondence = {
      val elem = articleHtml.select("p#correspondence")
      if (elem.size == 1) {
        elem.select("sup").remove
        Some(elem.get(0).text)
      } else if (elem.size > 1) {
        throw new Exception("!!!!!!! p#correspondence size > 1 !!!!!!!")
      } else if (elem.size == 0) {
        //"!!!!!!! no explicit correspondence (p#correspondence size == 0) !!!!!!!"
        None
      } else {
        throw new Exception("unexpected p#correspondence size")
      }
    }

    val authors = articleHtml
      .select("ol#authors > li")
      .toArray(Array[Element]())
      .map(authElem => {
      val orgIds = {
        if (authElem.children.size == 1) {
          val orgs = authElem.children.get(0).text.split(",")
          authElem.children.remove
          orgs
        } else if (authElem.children.size == 0) {
          Array[String]()
        } else {
          throw new Exception("unexpected authElem.children.size")
        }
      }

      val author = authElem.text.replace(",", "")
      (author, orgIds)
    })

    val emails = articleHtml
      .select("p#contactDetails > span.email")
      .toArray(Array[Element]())
      .map(e => e.text.replace(",", ""))

    /*
    val debugInfoWriter = new OutputStreamWriter(new FileOutputStream("debug.txt", true), "UTF-8")
    debugInfoWriter.write("----------------------------------------------------------\n")
    debugInfoWriter.write(index + ": " + articleURL + "\n")
    debugInfoWriter.write("\taffiliations:" + affiliations.map(aff => aff._1 + " --=> " + aff._2).mkString("\n\t\t", "\n\t\t", "\n"))
    debugInfoWriter.write("\tcorrespondence: " + correspondence + "\n")
    debugInfoWriter.write("\tauthors:" + authors.map({
      case (auth, orgIds) => auth + " - " + orgIds.mkString("{", ", ", "}")
    }).mkString("\n\t\t", "\n\t\t", "\n"))
    debugInfoWriter.write("\temails: " + emails.mkString("{", " | ", "}") + "\n")
    debugInfoWriter.close
    */

    val reportWriter = new OutputStreamWriter(new FileOutputStream("report.csv", true), "UTF-8")
    correspondence match {
      case Some(correspondence) =>
        val isEME = Bing.isEME(correspondence)
        emails.foreach(email => {
          //reportWriter.write(articleURL + "\t")
          //reportWriter.write((if (isEME) "EME\t" else "NOT EME\t"))
          //reportWriter.write(correspondence + "\t")
          //reportWriter.write(email + "\n")
          appendData(articleURL, Some(isEME), correspondence, email)
        })
      case None =>
        if (emails.isEmpty) {
          //reportWriter.write("NO CORRESPONDENCE\n") // EME field
          appendData(articleURL, None, null, null)
        } else {
          emails.foreach(email => {
            val author = authors.find({
              case (auth, _) => email.startsWith(auth)
            })
            author match {
              case Some((author, orgs)) =>
                /*
                if (affiliations.size == 1) {
                  if (affiliations(0)._1 != author)
                  val isEme = Bing.isEME(affiliations(0))
                  reportWriter.write((if (isEME) "EME\t" else "NOT EME\t"))
                  reportWriter.write(affiliations(0)._2 + "\t")
                  reportWriter.write(email + "\n")
                }
                */
                val affiliation = affiliations.find({
                  case (id, aff) => orgs.contains(id)
                })
                if (affiliation.isDefined) {
                  val isEME = Bing.isEME(affiliation.get._2)
                  //reportWriter.write(articleURL + "\t")
                  //reportWriter.write((if (isEME) "EME\t" else "NOT EME\t"))
                  //reportWriter.write(affiliation.get._2 + "\t")
                  //reportWriter.write(email + "\n")
                  appendData(articleURL, Some(isEME), affiliation.get._2, email)
                } else {
                  //throw new Exception("affiliation.isEmpty for " + articleURL)
                  appendData(articleURL, None, "NO AFFILIATION", email)
                }
              case None => ()
            }
          })
        }
    }
  }

  private def appendData(articleURL: String, isEme: Option[Boolean], affiliation: String, email: String) = {
    val reportWriter = new OutputStreamWriter(new FileOutputStream("report.csv", true), "UTF-8")
    reportWriter.write(articleURL + "\t")
    isEme match {
      case Some(x) => reportWriter.write(if (x) "EME\t" else "NOT EME\t")
      case None => reportWriter.write("NO CORRESPONDENCE\t")
    }
    if (affiliation != null) reportWriter.write(affiliation + "\t")
    if (email != null) reportWriter.write(email)
    reportWriter.write("\n")
    reportWriter.close
  }

  def getData(queryStr: String) = {
    //fetchAuthor(1, "http://onlinelibrary.wiley.com/doi/10.1111/j.2042-7158.2012.01492.x/abstract")
    //fetchAuthor(1, "http://onlinelibrary.wiley.com/doi/10.1002/jmr.977/abstract")
    //fetchAuthor(1, "http://onlinelibrary.wiley.com/doi/10.1002/wcms.89/abstract")

    val qryTemplate = "http://onlinelibrary.wiley.com/advanced/search/results/reentry?scope=allContent&dateRange=between&inTheLastList=6&startYear=2007&endYear=2012&queryStringEntered=false&searchRowCriteria[0].queryString=%s&searchRowCriteria[0].fieldName=all-fields&searchRowCriteria[0].booleanConnector=and&searchRowCriteria[1].fieldName=all-fields&searchRowCriteria[1].booleanConnector=and&searchRowCriteria[2].fieldName=all-fields&searchRowCriteria[2].booleanConnector=and&start=1&resultsPerPage=%d&ordering=relevancy"
    val (count, qry) = {
      val query = new URL(qryTemplate.format(queryStr, 5))
      val queryDetails = Jsoup.connect(query.toString).timeout(120000).get.select("p#searchedForText > em")
      (queryDetails.get(0).text.toInt, queryDetails.get(1).text)
    }
    println("There are " + count + " results for: '" + qry + "'")

    val query = qryTemplate.format(queryStr, count)
    println("query: " + query)
    print("Fetching articles URLs... ")
    val doiOfArticles = Jsoup
      .connect(query)
      .timeout(120000)
      .get
      .select("input[name=doi]")
    println("done")

    val urlOfArticles =
      doiOfArticles
        .toArray(Array[Element]())
        .map(doi => "http://onlinelibrary.wiley.com/doi/" + doi.attr("value") + "/abstract")

    urlOfArticles.zipWithIndex.foreach({
      case (articleURL, index) =>
        print((index + 1) + " of " + count + ": ")
        fetchAuthor(index, articleURL)
    })
  }
}