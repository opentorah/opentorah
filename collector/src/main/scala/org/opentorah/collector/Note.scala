package org.opentorah.collector

import org.opentorah.markdown.Markdown
import org.opentorah.store.{Directory, Context, Path, Viewer}
import org.opentorah.xml.{Attribute, Caching, Element, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO

final class Note(
  override val name: String,
  val title: Option[String]
) extends Directory.Entry(name), Viewer.Default:
  override def htmlHeadTitle: Option[String] = title
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)
  override def content(path: Path, context: Context): Caching.Parser[ScalaXml.Element] =
    Collector.get(context).notes.getFile(this).map(_.content)

object Note extends Element[Note]("note"), Directory.EntryMaker[Markdown, Note]:

  override def apply(name: String, markdown: Markdown): Parser[Note] = ZIO.succeed(new Note(
    name = name,
    title = markdown.title
  ))

  private val titleAttribute: Attribute.Optional[String] = Attribute("title").optional

  override def contentParsable: Parsable[Note] = new Parsable[Note]:
    override def parser: Parser[Note] = for
      name: String <- Directory.fileNameAttribute()
      title: Option[String] <- titleAttribute()
    yield new Note(
      name,
      title
    )

    override def unparser: Unparser[Note] = Unparser.concat(
      Directory.fileNameAttribute(_.name),
      titleAttribute(_.title)
    )
