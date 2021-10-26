package org.opentorah.collector

import org.opentorah.tei.Tei
import org.opentorah.store.{By, Context, Path, Pure, Viewer}
import org.opentorah.util.Effects
import org.opentorah.xml.{Caching, Element, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO

final class EntityLists(
  selectorName: String,
  val lists: Seq[EntityList]
) extends
  By.WithSelector[EntityList](selectorName),
  Pure[EntityList],
  Viewer.Apparatus:
  
  def setUp(collector: Collector): Caching.Parser[Unit] = for
    entities: Seq[Entity] <- collector.entities.stores
  yield
    for list <- lists do list.setEntities(
      entities.filter(entity =>
        (entity.entityType == list.entityType) &&
        (entity.role       == list.role      )
      )
    )

  override def storesPure: Seq[EntityList] = lists

  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  override def content(path: Path, context: Context): Caching.Parser[ScalaXml.Element] =
    val nonEmptyLists: Seq[EntityList] = lists.filter(_.getEntities.nonEmpty)
    for pathShortener: Path.Shortener <- context.pathShortener yield
    <div>
      <p>{for list <- nonEmptyLists yield
        // TODO why is it path and entityListPath(list)?
        <l>
          {a(path, pathShortener).setFragment(list.names.name)(xml = list.title)}
          {a(context.path(list), pathShortener)(EntityLists.expand)}
        </l>
      }</p>
      {for list <- nonEmptyLists yield
      <list id={list.names.name}>
        <head xmlns={Tei.namespace.uri}>
          {list.title.content.scalaXml}
          {a(context.path(list), pathShortener)(EntityLists.expand)}
        </head>
        {for entity <- list.getEntities yield entity.line(context, pathShortener)}
      </list>
    }</div>


object EntityLists extends Element[EntityLists]("entityLists"):
  // TODO gather all arrows and other symbols in one place
  val expand: String = "⇗"

  override def contentParsable: Parsable[EntityLists] = new Parsable[EntityLists]:
    override def parser: Parser[EntityLists] = for
      selectorName: String <- By.selectorParser
      lists: Seq[EntityList] <- EntityList.seq()
    yield EntityLists(
      selectorName,
      lists
    )

    override def unparser: Unparser[EntityLists] = Unparser.concat(
      By.selectorUnparser,
      EntityList.seq(_.lists)
    )
