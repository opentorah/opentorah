package org.opentorah.collector

import org.opentorah.tei.{Entity => TeiEntity}
import org.opentorah.xml.{Unparser, Element, FromUrl, Parsable, Parser}
import java.net.URL

final class Entities(
  override val fromUrl: FromUrl,
  override val selector: Selector,
  override val directory: String
) extends Directory[TeiEntity, Entity, Entities.All](
  directory,
  "xml",
  Entity,
  new Entities.All(_)
) with By {

  override protected def loadFile(url: URL): Parser[TeiEntity] = TeiEntity.parse(url)

  override def findByName(name: String): Caching.Parser[Option[Entity]] = getDirectory >>= (_.findByName(name))
}

object Entities extends Element[Entities]("entities") {

  final class All(name2entry: Map[String, Entity]) extends Directory.Wrapper[Entity](name2entry)

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
