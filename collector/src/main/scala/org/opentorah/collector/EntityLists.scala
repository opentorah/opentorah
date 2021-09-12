package org.opentorah.collector

import org.opentorah.tei.Tei
import org.opentorah.site.HtmlContent
import org.opentorah.store.{By, Caching, Selector, Stores}
import org.opentorah.util.Effects
import org.opentorah.xml.{Element, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO

final class EntityLists(
  override val selector: Selector,
  val lists: Seq[EntityList]
) extends By, Stores.Pure, HtmlContent[Collector]:
  
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

  override def content(collector: Collector): Caching.Parser[ScalaXml.Element] = ZIO.succeed {
    val nonEmptyLists: Seq[EntityList] = lists.filter(_.getEntities.nonEmpty)
    <div>
      <p>{nonEmptyLists.map(list =>
        <l>
          {a(collector).setFragment(list.names.name)(xml = list.title)}
          {list.a(collector)("\uF065")}
        </l>
      )}</p>
      {nonEmptyLists.map(list =>
      <list id={list.names.name}>
        <head xmlns={Tei.namespace.uri}>
          {list.title.content}
          {list.a(collector)("\uF065")}
        </head>
        {list.getEntities.map(entity => Entity.line(entity, collector))}
      </list>
    )}</div>
  }

object EntityLists extends Element[EntityLists]("entityLists"):

  override def contentParsable: Parsable[EntityLists] = new Parsable[EntityLists]:
    override def parser: Parser[EntityLists] = for
      selector: Selector <- By.selectorParser
      lists: Seq[EntityList] <- EntityList.seq()
    yield EntityLists(
      selector,
      lists
    )

    override def unparser: Unparser[EntityLists] = Unparser.concat(
      By.selectorUnparser,
      EntityList.seq(_.lists)
    )
