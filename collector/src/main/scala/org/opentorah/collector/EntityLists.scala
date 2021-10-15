package org.opentorah.collector

import org.opentorah.tei.Tei
import org.opentorah.site.HtmlContent
import org.opentorah.store.{By, Caching, Store, Stores}
import org.opentorah.util.Effects
import org.opentorah.xml.{Element, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO

final class EntityLists(
  selectorName: String,
  val lists: Seq[EntityList]
) extends By.WithSelector[EntityList](selectorName), Stores.Pure[EntityList], HtmlContent.ApparatusViewer[Collector]:
  
  def setUp(collector: Collector): Caching.Parser[Unit] =
    collector.entities.stores.map(allEntities =>
      for list <- lists do list.setEntities(
        allEntities.filter(entity =>
          (entity.entityType == list.entityType) &&
          (entity.role       == list.role      )
        )
      )
    )

  override def storesPure: Seq[EntityList] = lists

  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  override def content(path: Store.Path, collector: Collector): Caching.Parser[ScalaXml.Element] = ZIO.succeed {
    val nonEmptyLists: Seq[EntityList] = lists.filter(_.getEntities.nonEmpty)
    <div>
      <p>{for list <- nonEmptyLists yield
        // TODO why is it path and entityListPath(list)?
        <l>
          {HtmlContent.a(path).setFragment(list.names.name)(xml = list.title)}
          {HtmlContent.a(collector.entityListPath(list))(EntityLists.expand)}
        </l>
      }</p>
      {for list <- nonEmptyLists yield
      <list id={list.names.name}>
        <head xmlns={Tei.namespace.uri}>
          {list.title.content}
          {HtmlContent.a(collector.entityListPath(list))(EntityLists.expand)}
        </head>
        {for entity <- list.getEntities yield Entity.line(entity, collector)}
      </list>
    }</div>
  }

object EntityLists extends Element[EntityLists]("entityLists"):

  val expand: String = "\uF065"

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
