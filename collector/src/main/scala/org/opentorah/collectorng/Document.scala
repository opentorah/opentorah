package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Author, Editor, EntityReference, Page, Pb, Tei}
import org.opentorah.xml.{Antiparser, Attribute, Parser}

final class Document(
  override val name: String,
  val lang: String,
  val editors: Seq[Editor],
  val description: Option[Abstract.Value],
  val date: Option[String],
  val authors: Seq[Author.Value],
  val addressee: Option[EntityReference],
  val pbs: Seq[Pb]
) extends Directory.Entry(name) {
  def pages(pageType: Page.Type): Seq[Page] =
    for (pb <- pbs) yield pageType(pb)
}

object Document extends Directory.EntryMaker[Tei, Document]("document") {

  final class Facet(val document: Document, val facet: Collection.Facet) extends Store {
    override def findByName(name: String): Option[Store] = None
    override def names: Names = Names(withExtension)
    def withExtension: String = document.name + "." + facet.extension
  }

  override def apply(name: String, tei: Tei): Document = {
    val lang: Option[String] = tei.text.lang

    val (_: String, language: Option[String]) = splitLang(name)
    if (language.isDefined && lang != language)
      throw new IllegalArgumentException(s"Wrong language in $name: $lang != $language")

    new Document(
      name,
      lang.get,
      tei.editors,
      description = tei.getAbstract,
      date = tei.creationDate.map(_.when),
      tei.authors,
      tei.addressee,
      tei.pbs
    )
  }

  private def splitLang(name: String): (String, Option[String]) = {
    val dash: Int = name.lastIndexOf('-')
    if ((dash == -1) || (dash != name.length-3)) (name, None)
    else (name.substring(0, dash), Some(name.substring(dash+1)))
  }

  private val langAttribute: Attribute[String] = Attribute("lang")
  private val dateAttribute: Attribute[String] = Attribute("date")

  override def parser: Parser[Document] = for {
    name <- Directory.fileName
    lang <- langAttribute.required
    editors <- Editor.all
    description <- Abstract.parsable.optional
    date <- dateAttribute.optional
    authors <- Author.parsable.all
    addressee <- EntityReference.optional
    pbs <- Pb.all
  } yield new Document(
    name,
    lang,
    editors,
    description,
    date,
    authors,
    addressee,
    pbs
  )

  override def antiparser: Antiparser[Document] = Antiparser.concat(
    Directory.fileNameToXml,
    langAttribute.toXml(_.lang),
    Editor.toXmlSeq(_.editors),
    Abstract.parsable.toXmlOption(_.description),
    dateAttribute.toXmlOption(_.date),
    Author.parsable.toXmlSeq(_.authors),
    EntityReference.toXmlOption(_.addressee),
    Pb.toXmlSeq(_.pbs)
  )
}
