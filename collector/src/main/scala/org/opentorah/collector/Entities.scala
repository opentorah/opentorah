package org.opentorah.collector

import org.opentorah.tei.{Entity => TeiEntity}
import org.opentorah.xml.{Unparser, Element, FromUrl, Parsable, Parser}
import java.net.URL

final class Entities(
  override val fromUrl: FromUrl,
  override val selector: Selector,
  override val directory: String
) extends Directory[TeiEntity, Entity, Map[String, Entity]](directory, "xml", Entity, identity) with By {

  override protected def loadFile(url: URL): TeiEntity = Parser.parseDo(TeiEntity.parse(url))

  private lazy val name2entity: Map[String, Entity] = getDirectory

  override def findByName(name: String): Option[Entity] = findByName(name, name2entity)

  override def directoryEntries: Seq[Entity] = name2entity.values.toSeq

  def entities: Seq[Entity] = directoryEntries
}

object Entities extends Element[Entities]("entities") {

  override def contentParsable: Parsable[Entities] = new Parsable[Entities] {
    override def parser: Parser[Entities] = for {
      fromUrl <- Element.currentFromUrl
      selector <- By.selectorParser
      directory <- Directory.directoryAttribute()
    } yield new Entities(
      fromUrl,
      selector,
      directory
    )

    override def unparser: Unparser[Entities] = Unparser.concat(
      By.selectorUnparser,
      Directory.directoryAttribute(_.directory)
    )
  }
}
