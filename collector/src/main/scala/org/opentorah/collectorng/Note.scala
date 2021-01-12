package org.opentorah.collectorng

import org.opentorah.xml.{Unparser, Attribute, Parsable, Parser, Xml}

final class Note(
  override val name: String,
  val title: Option[String]
) extends Directory.Entry(name) with HtmlContent {

  override def viewer: Html.Viewer = Html.Viewer.default
  override def isWide: Boolean = false
  override def htmlTitle: Option[String] = title
  override def navigationLinks: Seq[Html.NavigationLink] = Seq.empty
  override def lang: Option[String] = None
  override def content(site: Site): Xml.Element = site.byNote.getFile(name).html
}

object Note extends Directory.EntryMaker[Markdown, Note]("note") {

  override def apply(name: String, markdown: Markdown): Note = new Note(
    name,
    markdown.title
  )

  private val titleAttribute: Attribute.Optional[String] = Attribute("title").optional

  override def contentParsable: Parsable[Note] = new Parsable[Note] {
    override def parser: Parser[Note] = for {
      name <- Directory.fileNameAttribute()
      title <- titleAttribute()
    } yield new Note(
      name,
      title
    )

    override def unparser: Unparser[Note] = Unparser.concat(
      Directory.fileNameAttribute(_.name),
      titleAttribute(_.title)
    )
  }
}
