package org.opentorah.collector

import org.opentorah.markdown.Markdown
import org.opentorah.site.HtmlContent
import org.opentorah.store.{Caching, Directory}
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO

final class Note(
  override val name: String,
  val title: Option[String]
) extends Directory.Entry(name) with HtmlContent[Collector] {

  override def htmlHeadTitle: Option[String] = title
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)
  override def content(collector: Collector): Caching.Parser[ScalaXml.Element] = collector.notes.getFile(this).map(_.content)
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
