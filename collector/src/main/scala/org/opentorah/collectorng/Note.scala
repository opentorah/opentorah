package org.opentorah.collectorng

import org.opentorah.xml.{Antiparser, Attribute, Parser}

final class Note(
  override val name: String,
  val title: Option[String]
) extends Directory.Entry(name)

object Note extends Directory.EntryMaker[Markdown, Note]("note") {

  override def apply(name: String, markdown: Markdown): Note = new Note(
    name,
    markdown.title
  )

  private val titleAttribute: Attribute[String] = Attribute("title")

  override def parser: Parser[Note] = for {
    name <- Directory.fileName
    title <- titleAttribute.optional
  } yield new Note(
    name,
    title
  )

  override def antiparser: Antiparser[Note] = Antiparser.concat(
    Directory.fileNameToXml,
    titleAttribute.toXmlOption(_.title)
  )
}
