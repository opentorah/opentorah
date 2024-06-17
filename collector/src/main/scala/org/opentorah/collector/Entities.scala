package org.opentorah.collector

import org.opentorah.tei.Entity as TeiEntity
import org.opentorah.store.{By, Context, Directory, Path}
import org.opentorah.xml.{Atom, Element, ElementTo, Elements, From, FromUrl, Nodes, Parsable, Parser, Unparser}
import java.net.URL
import zio.ZIO

final class Entities(
  override val fromUrl: FromUrl,
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
  override def htmlBodyTitle: Option[Nodes] = htmlHeadTitle.map(Atom.apply)

  override def content(path: Path, context: Context): Parser[Element] = for
    allEntities: Seq[Entity] <- stores
    lines: Elements <- ZIO.foreach(Entity.sort(allEntities))((entity: Entity) =>
      entity.line(context)
    )
  yield
    <list>{lines}</list>

object Entities extends ElementTo[Entities]("entities"):

  final class All(name2entry: Map[String, Entity]) extends Directory.Wrapper[Entity](name2entry)

  override def contentParsable: Parsable[Entities] = new Parsable[Entities]:
    override def parser: Parser[Entities] = for
      fromUrl: FromUrl <- FromUrl.get
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
