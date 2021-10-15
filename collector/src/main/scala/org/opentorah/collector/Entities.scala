package org.opentorah.collector

import org.opentorah.site.HtmlContent
import org.opentorah.tei.{Entity as TeiEntity}
import org.opentorah.store.{By, Caching, Directory, Store}
import org.opentorah.xml.{Element, Parsable, Parser, ScalaXml, Unparser}
import java.net.URL

final class Entities(
  override val fromUrl: Element.FromUrl,
  selectorName: String,
  override val directory: String
) extends Directory[TeiEntity, Entity, Entities.All](
  directory,
  "xml",
  Entity,
  Entities.All(_)
), By.WithSelector[Entity](selectorName), HtmlContent.ApparatusViewer[Collector]:

  override protected def loadFile(url: URL): Parser[TeiEntity] = TeiEntity.parse(url, ScalaXml)

  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  override def content(path: Store.Path, collector: Collector): Caching.Parser[ScalaXml.Element] = stores.map(allEntities =>
    <list>
      {for entity <- Entity.sort(allEntities) yield Entity.line(entity, collector)}
    </list>
  )

object Entities extends Element[Entities]("entities"):

  final class All(name2entry: Map[String, Entity]) extends Directory.Wrapper[Entity](name2entry)

  override def contentParsable: Parsable[Entities] = new Parsable[Entities]:
    override def parser: Parser[Entities] = for
      fromUrl: Element.FromUrl <- Element.fromUrl
      selectorName: String <- By.selectorParser
      directory: String <- Directory.directoryAttribute()
    yield Entities(
      fromUrl,
      selectorName,
      directory
    )

    override def unparser: Unparser[Entities] = Unparser.concat(
      By.selectorUnparser,
      Directory.directoryAttribute(_.directory)
    )
