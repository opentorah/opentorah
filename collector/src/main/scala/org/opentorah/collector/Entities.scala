package org.opentorah.collector

import org.opentorah.tei.{Entity => TeiEntity}
import org.opentorah.store.{By, Caching, Directory, Selector, Store}
import org.opentorah.xml.{Element, FromUrl, Parsable, Parser, Unparser}
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

  override def stores: Seq[Store] = Seq.empty // TODO not really used here, and should probably be a Parser...

  override def findByName(name: String): Caching.Parser[Option[Entity]] = getDirectory.flatMap(_.findByName(name))
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
