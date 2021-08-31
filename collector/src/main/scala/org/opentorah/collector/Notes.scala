package org.opentorah.collector

import org.opentorah.markdown.Markdown
import org.opentorah.site.HtmlContent
import org.opentorah.store.{By, Caching, Directory, Selector, Store}
import org.opentorah.xml.{Element, FromUrl, Parsable, Parser, ScalaXml, Unparser}
import zio.UIO
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
) with By with HtmlContent[Collector] {

  override protected def loadFile(url: URL): UIO[Markdown] = UIO.succeed(Markdown(url))

  override def stores: Seq[Store] = Seq.empty // TODO not really used here, and should probably be a Parser...

  override def findByName(name: String): Caching.Parser[Option[Store]] = findByName(
    fullName = name,
    findByName = name => getDirectory.flatMap(_.findByName(name)),
    allowedExtension = "html",
    assumeAllowedExtension = false
  )

  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  override def content(site: Collector): Caching.Parser[ScalaXml.Element] = directoryEntries.map(notes =>
    <div>{notes.sortBy(_.title).map(note => <l>{note.a(site)(text = note.title.getOrElse("NO TITLE"))}</l>)}</div>)
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
