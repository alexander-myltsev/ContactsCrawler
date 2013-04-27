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
    case class Affiliation(id: String, name: String)
    val affiliations = articleHtml
      .select("li.affiliation")
      .toArray(Array[Element]())
      .map(organizationElem => {
      val id = organizationElem.select("sup").text
      val name = organizationElem.children.select("p").text
      Affiliation(id, name)
    })

    val correspondenceOpt = {
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

    case class Author(name: String, affilationIds: Array[String]) {
      override def toString() = "Author(" + name + ", " + affilationIds.mkString("{", ", ", "}") + ")"
    }

    val authors = articleHtml
      .select("ol#authors > li")
      .toArray(Array[Element]())
      .map(authElem => {
      val affIds = {
        if (authElem.children.size == 1) {
          val orgs = authElem.children.get(0).text.split(",")
          authElem.children.remove
          orgs
        } else if (authElem.children.size == 0) {
          Array("")
        } else {
          throw new Exception("unexpected authElem.children.size")
        }
      }

      val authName = authElem.text.replace(",", "")
      Author(authName, affIds)
    })

    val emails = articleHtml
      .select("p#contactDetails > span.email")
      .toArray(Array[Element]())
      .map(e => e.text.replace(",", ""))

    println("\tauthors:\n\t\t" + authors.mkString("\n\t\t"))
    println("\taffilations:\n\t\t" + affiliations.mkString("\n\t\t"))

    //val reportWriter = new OutputStreamWriter(new FileOutputStream("report.csv", true), "UTF-8")
    // Process correspondence
    println("\tProcessing correspondence: " + correspondenceOpt)
    println("\temails: " + emails.mkString(" | "))
    correspondenceOpt match {
      case Some(correspondence) =>
        val isEME = Bing.isEME(correspondence)
        println("\tisEME: " + isEME)
        emails.foreach(email => {
          appendCorrespondenceData(articleURL, Some(isEME), correspondence, email)
        })
      case None =>
        if (emails.isEmpty) {
          appendCorrespondenceData(articleURL, None, null, null)
        } else {
          emails.foreach(email => {
            val author = authors.find({
              case Author(name, _) => email.startsWith(name)
            })
            author match {
              case Some(Author(author, orgs)) =>
                val affiliationOpt = affiliations.find({
                  case Affiliation(id, aff) => orgs.contains(id)
                })
                affiliationOpt match {
                  case Some(affiliation) =>
                    val isEME = Bing.isEME(affiliation.name)
                    appendCorrespondenceData(articleURL, Some(isEME), affiliation.name, email)
                  case None =>
                    appendCorrespondenceData(articleURL, None, "NO AFFILIATION", email)
                }
              case None => ()
            }
          })
        }
    }

    // Authors homepages feature:
    authors.foreach(author => {
      val affiliationsEME = author.affilationIds.foldLeft(List[Affiliation]())({
        case (res, affId) =>
          affiliations.find(_.id == affId) match {
            case Some(x) => if (Bing.isEME(x.name)) x :: res else res
            case None => res
          }
      })
      affiliationsEME.foreach(affEme => {
        val possibleHomepages = BingSearch.getData("\"" + author.name + "\" homepage " + affEme.name)
        //println("Response: " + resp)
        val reportWriter = new OutputStreamWriter(new FileOutputStream("report.csv", true), "UTF-8")
        reportWriter.write(articleURL + "\t")
        reportWriter.write("EME\t")
        reportWriter.write(affEme.name + "\t")
        reportWriter.write(author.name + "\t")
        possibleHomepages.foreach(posshp => {
          reportWriter.write(posshp.toString + "\t")
        })
        reportWriter.write("\n")
        reportWriter.close
      })
      //println("========================")
      //println("\t" + author.name + " affiliationsEME:\n\t\t" + affiliationsEME.mkString("\n\t\t"))
      //println("========================")
    })

    println("--------------------")
  }

  private def appendCorrespondenceData(articleURL: String, isEme: Option[Boolean], affiliation: String, email: String) = {
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
    //fetchAuthor(4, "http://onlinelibrary.wiley.com/doi/10.1002/pssr.201105241/abstract")
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