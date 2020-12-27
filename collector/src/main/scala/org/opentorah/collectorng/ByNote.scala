package org.opentorah.collectorng

import org.opentorah.xml.{Antiparser, Element, FromUrl, Parser}
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

  override def parser: Parser[ByNote] = for {
    fromUrl <- currentFromUrl
    selector <- By.selector
    directory <- Directory.directory
  } yield new ByNote(
    fromUrl,
    selector,
    directory
  )

  override def antiparser: Antiparser[ByNote] = Antiparser.concat(
    By.selectorToXml,
    Directory.directoryToXml
  )
}
