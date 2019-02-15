package org.podval.archive19kislev.collector

import org.podval.archive19kislev.collector.Xml.Ops

import scala.xml.Elem


final class Document(xml: Elem, val name: String) {

  private[this] val tei: Elem = Xml.open(xml, "TEI")
  private[this] val teiHeader: Elem = tei.oneChild("teiHeader")
  private[this] val fileDesc: Elem = teiHeader.oneChild("fileDesc")
  private[this] val titleStmt: Elem = fileDesc.oneChild("titleStmt")

  val (partTitle: Option[String], title: Option[String], subTitle: Option[String]) = extractTitles(titleStmt)

  val author: Option[String] = optionize(titleStmt.oneChild("author").text)

  val date: Option[String] = optionize(fileDesc.oneChild("editionStmt").oneChild("edition").oneChild("date").text)

  val publicationDate: Option[String] = optionize(fileDesc.oneChild("publicationStmt").oneChild("date").text)

  val langUsage: Elem = teiHeader.oneChild("profileDesc").oneChild("langUsage")
  val languages: Seq[String] = extractLanguages(langUsage)
  val language: Option[String] = languages.headOption

  val content: Seq[Elem] = tei.oneChild("text").oneChild("body").elems


  private[this] def extractTitles(titleStmt: Elem): (Option[String], Option[String], Option[String]) = {
    def text(e: Elem): String =   (e.child map (_.text)).mkString(" ")

    val titles = titleStmt.elemsFilter("title")
    val partTitle = titles.find(_.getAttribute("type") == "part").map(text)
    val mainTitle = titles.find(_.getAttribute("type") == "main").map(text).flatMap(optionize)
    val subTitle = titles.find(_.getAttribute("type") == "sub").map(text)
    (partTitle, mainTitle, subTitle)
  }


  private[this] def extractLanguages(langUsage: Elem): Seq[String] = langUsage.elems("language").map(_.getAttribute("ident"))


  private[this] def optionize(what: String): Option[String] = if (what.isEmpty || (what == "?")) None else Some(what)


  val pages: Seq[Page] = {
    val pbs: Seq[Elem] = content.flatMap(_ \\ "pb").filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])
    for {
      pb <- pbs
      name = pb.getAttribute("n")
      isPresent = pb.attributeOption("facs").isDefined
    } yield new Page(name, isPresent, this)
  }


  def indexTableRows: Seq[Elem] = {
    (if (partTitle.isDefined) Seq(partRow) else Seq.empty) ++ Seq(documentRow)
  }


  private[this] def partRow: Elem = {
    <tr>
      <td class="part-title" colspan="6">{partTitle.get}</td>
    </tr>
  }


  private[this] def documentRow: Elem = {
    <tr>
      <td>{title.getOrElse("?")}</td>
      <td>{date.getOrElse("")}</td>
      <td>{author.getOrElse("")}</td>
      <td>{language.getOrElse("")}</td>
      <td><a href={s"documents/$name.xml"}>{name}</a></td>
      <td>{for (page <- pages) yield page.pageReference}</td>
    </tr>
  }
}



object Document {

  val indexTableHeader: Elem =
    <tr>
      <th class="description">Описание</th>
      <th class="date">Дата</th>
      <th class="author">Автор</th>
      <th class="language">Язык</th>
      <th class="document">Документ</th>
      <th class="pages">Страницы</th>
    </tr>
}
