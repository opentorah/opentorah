package org.opentorah.collector

import org.opentorah.html.A
import org.opentorah.site.Markdown
import org.opentorah.store.{By, Context, Directory, Path}
import org.opentorah.xml.{Atom, Element, ElementTo, Elements, FromUrl, Nodes, Parsable, Parser, Unparser}
import zio.{UIO, ZIO}
import java.net.URL

final class Notes(
  override val fromUrl: FromUrl,
  selectorName: String,
  override val directory: String
) extends
  Directory[Markdown, Note, Notes.All](
    directory,
    "md",
    Note,
    Notes.All(_),
  ),
  By.WithSelector[Note](selectorName):

  override protected def loadFile(url: URL): UIO[Markdown] = ZIO.succeed(Markdown(url))

  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[Nodes] = htmlHeadTitle.map(Atom.apply)

  override def content(path: Path, context: Context): Parser[Element] = for
    notes: Seq[Note] <- stores
    lines: Elements <- ZIO.foreach(notes.sortBy(_.title))((note: Note) =>
      for a: A <- context.a(path :+ note) yield
        <l>{a(text = note.title.getOrElse("NO TITLE"))}</l>
    )
  yield <div>{lines}</div>

object Notes extends ElementTo[Notes]("notes"):

  final class All(name2entry: Map[String, Note]) extends Directory.Wrapper[Note](name2entry)

  override def contentParsable: Parsable[Notes] = new Parsable[Notes]:
    override def parser: Parser[Notes] = for
      fromUrl: FromUrl <- FromUrl.get
      selectorName: String <- By.selectorParser
      directory: String <- Directory.directoryAttribute()
    yield Notes(
      fromUrl,
      selectorName,
      directory
    )

    override def unparser: Unparser[Notes] = Unparser.concat(
      By.selectorUnparser,
      Directory.directoryAttribute(_.directory)
    )
