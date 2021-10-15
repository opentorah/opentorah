package org.opentorah.collector

import org.opentorah.markdown.Markdown
import org.opentorah.site.HtmlContent
import org.opentorah.store.{By, Caching, Directory, Store}
import org.opentorah.xml.{Element, Parsable, Parser, ScalaXml, Unparser}
import zio.UIO
import java.net.URL

final class Notes(
  override val fromUrl: Element.FromUrl,
  selectorName: String,
  override val directory: String
) extends Directory[Markdown, Note, Notes.All](
  directory,
  "md",
  Note,
  Notes.All(_),
), By.WithSelector[Note](selectorName), HtmlContent.DefaultViewer[Collector]:

  override protected def loadFile(url: URL): UIO[Markdown] = UIO.succeed(Markdown(url))

  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  override def content(path: Store.Path, collector: Collector): Caching.Parser[ScalaXml.Element] = stores.map(notes =>
    <div>{for note <- notes.sortBy(_.title) yield <l>{HtmlContent.a(path :+ note)(text = note.title.getOrElse("NO TITLE"))}</l>}</div>)

object Notes extends Element[Notes]("notes"):

  final class All(name2entry: Map[String, Note]) extends Directory.Wrapper[Note](name2entry)

  override def contentParsable: Parsable[Notes] = new Parsable[Notes]:
    override def parser: Parser[Notes] = for
      fromUrl: Element.FromUrl <- Element.fromUrl
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
