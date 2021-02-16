package org.opentorah.collector

import org.opentorah.markdown.Markdown
import org.opentorah.xml.{Element, FromUrl, Parsable, Parser, Unparser, Xml}
import java.net.URL

final class Notes(
  override val fromUrl: FromUrl,
  override val selector: Selector,
  override val directory: String
) extends Directory[Markdown, Note, Notes.All](
  directory,
  "md",
  Note,
  new Notes.All(_),
) with By with HtmlContent {

  override protected def loadFile(url: URL): Markdown = Markdown(url)

  override def findByName(name: String): Option[Store] =
    Store.findByName(name, "html", getDirectory.findByName)

  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[Xml.Nodes] = htmlHeadTitle.map(Xml.mkText)
  override def acceptsIndexHtml: Boolean = true

  override def path(site: Site): Store.Path = Seq(site.notes)

  override def content(site: Site): Xml.Element =
    <div>{for (note <- directoryEntries) yield <l>{note.a(site)(text = note.title.getOrElse("NO TITLE"))}</l>}</div>
}

object Notes extends Element[Notes]("notes") {

  final class All(name2entry: Map[String, Note]) extends Directory.Wrapper[Note](name2entry)

  override def contentParsable: Parsable[Notes] = new Parsable[Notes] {
    override def parser: Parser[Notes] = for {
      fromUrl <- Element.currentFromUrl
      selector <- By.selectorParser
      directory <- Directory.directoryAttribute()
    } yield new Notes(
      fromUrl,
      selector,
      directory
    )

    override def unparser: Unparser[Notes] = Unparser.concat(
      By.selectorUnparser,
      Directory.directoryAttribute(_.directory)
    )
  }
}
