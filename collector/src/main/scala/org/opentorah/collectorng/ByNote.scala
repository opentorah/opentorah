package org.opentorah.collectorng

import org.opentorah.xml.{Unparser, Element, FromUrl, Parsable, Parser}
import java.net.URL

final class ByNote(
  override val fromUrl: FromUrl,
  override val selector: Selector,
  override val directory: String
) extends Directory[Markdown, Note](directory, "md", Note) with By {

  override protected def loadFile(url: URL): Markdown = Markdown.load(url)

  private lazy val name2note: Map[String, Note] = readDirectory

  override def findByName(name: String): Option[Store] = findByName(name, name2note)

  override def directoryEntries: Seq[Note] = notes
  def notes: Seq[Note] = name2note.values.toSeq
}

object ByNote extends Element[ByNote]("byNote") {

  override def contentParsable: Parsable[ByNote] = new Parsable[ByNote] {
    override def parser: Parser[ByNote] = for {
      fromUrl <- Element.currentFromUrl
      selector <- By.selector
      directory <- Directory.directoryAttribute()
    } yield new ByNote(
      fromUrl,
      selector,
      directory
    )

    override def unparser: Unparser[ByNote] = Unparser.concat(
      By.selectorToXml,
      Directory.directoryAttribute(_.directory)
    )
  }
}
