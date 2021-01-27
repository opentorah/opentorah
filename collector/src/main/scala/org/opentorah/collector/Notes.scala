package org.opentorah.collector

import org.opentorah.xml.{Element, FromUrl, Parsable, Parser, Unparser, Xml}
import java.net.URL

final class Notes(
  override val fromUrl: FromUrl,
  override val selector: Selector,
  override val directory: String
) extends Directory[Markdown, Note, Map[String, Note]](
  directory,
  "md",
  Note,
  identity
) with By with HtmlContent {

  override protected def loadFile(url: URL): Markdown = Markdown.load(url)

  private lazy val name2note: Map[String, Note] = getDirectory

  override def findByName(name: String): Option[Store] =
    Store.checkExtension(name, "html")
      .flatMap(name => findByName(name, name2note))

  override def directoryEntries: Seq[Note] = notes
  def notes: Seq[Note] = name2note.values.toSeq

  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[Seq[Xml.Node]] = htmlHeadTitle.map(Xml.mkText)
  override def acceptsIndexHtml: Boolean = true

  override def path(site: Site): Store.Path = Seq(site.notes)

  override def content(site: Site): Xml.Element =
    <div>{for (note <- notes) yield <l>{note.a(site)(text = note.title.getOrElse("NO TITLE"))}</l>}</div>
}

object Notes extends Element[Notes]("notes") {

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
