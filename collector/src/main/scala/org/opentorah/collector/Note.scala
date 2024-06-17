package org.opentorah.collector

import org.opentorah.site.Markdown
import org.opentorah.store.{Context, Directory, Path}
import org.opentorah.xml.{Atom, Attribute, Element, ElementTo, Nodes, Parsable, Parser, Unparser}
import zio.ZIO

final class Note(
  override val name: String,
  val title: Option[String]
) extends Directory.Entry(name):
  override def htmlHeadTitle: Option[String] = title
  override def htmlBodyTitle: Option[Nodes] = htmlHeadTitle.map(Atom.apply)
  override def content(path: Path, context: Context): Parser[Element] =
    Collector.get(context).notes.getFile(this).map(_.content)

object Note extends ElementTo[Note]("note"), Directory.EntryMaker[Markdown, Note]:

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
