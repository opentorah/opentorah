package org.opentorah.collector

import org.opentorah.markdown.Markdown
import org.opentorah.site.{Caching, Directory, HtmlContent}
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser, Xml}
import zio.ZIO

final class Note(
  override val name: String,
  val title: Option[String]
) extends Directory.Entry(name) with HtmlContent[Site] {

  override def htmlHeadTitle: Option[String] = title
  override def htmlBodyTitle: Option[Xml.Nodes] = htmlHeadTitle.map(Xml.mkText)
  override def content(site: Site): Caching.Parser[Xml.Element] = site.notes.getFile(this).map(_.content)
}

object Note extends Element[Note]("note") with Directory.EntryMaker[Markdown, Note] {

  override def apply(name: String, markdown: Markdown): Parser[Note] = ZIO.succeed(new Note(
    name,
    markdown.title
  ))

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
