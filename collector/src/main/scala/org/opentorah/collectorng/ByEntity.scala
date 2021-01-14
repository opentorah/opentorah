package org.opentorah.collectorng

import org.opentorah.tei.{Entity => TeiEntity}
import org.opentorah.xml.{Unparser, Element, FromUrl, Parsable, Parser}
import java.net.URL

final class ByEntity(
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

object ByEntity extends Element[ByEntity]("byEntity") {

  override def contentParsable: Parsable[ByEntity] = new Parsable[ByEntity] {
    override def parser: Parser[ByEntity] = for {
      fromUrl <- Element.currentFromUrl
      selector <- By.selectorParser
      directory <- Directory.directoryAttribute()
    } yield new ByEntity(
      fromUrl,
      selector,
      directory
    )

    override def unparser: Unparser[ByEntity] = Unparser.concat(
      By.selectorUnparser,
      Directory.directoryAttribute(_.directory)
    )
  }
}
