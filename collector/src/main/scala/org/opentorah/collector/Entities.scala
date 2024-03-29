package org.opentorah.collector

import org.opentorah.tei.Entity as TeiEntity
import org.opentorah.store.{By, Context, Directory, Path, Store}
import org.opentorah.xml.{Caching, Element, From, Parsable, Parser, ScalaXml, Unparser}
import java.net.URL
import zio.ZIO

final class Entities(
  override val fromUrl: Element.FromUrl,
  selectorName: String,
  override val directory: String
) extends
  Directory[TeiEntity, Entity, Entities.All](
    directory,
    "xml",
    Entity,
    Entities.All(_)
  ),
  By.WithSelector[Entity](selectorName):

  override protected def loadFile(url: URL): Parser[TeiEntity] = TeiEntity.parse(From.url(url))

  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  override def content(path: Path, context: Context): Caching.Parser[ScalaXml.Element] = for
    allEntities: Seq[Entity] <- stores
    lines: Seq[ScalaXml.Element] <- ZIO.foreach(Entity.sort(allEntities))((entity: Entity) =>
      entity.line(context)
    )
  yield
    <list>{lines}</list>

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
