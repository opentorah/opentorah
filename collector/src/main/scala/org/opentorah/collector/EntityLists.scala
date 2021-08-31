package org.opentorah.collector

import org.opentorah.tei.Tei
import org.opentorah.site.HtmlContent
import org.opentorah.store.{By, Caching, Selector, Stores}
import org.opentorah.util.Effects
import org.opentorah.xml.{Element, Parsable, Parser, ScalaXml, Unparser}

final class EntityLists(
  override val selector: Selector,
  val lists: Seq[EntityList]
) extends By with Stores.NonTerminal with HtmlContent[Collector] {

  private var setupDone: Boolean = false

  def setUp(collector: Collector): Caching.Parser[Unit] = if (setupDone) Effects.ok else
    collector.entities.directoryEntries.map { allEntities =>
      for (list <- lists) list.setEntities(
        allEntities.filter(entity =>
          (entity.entityType == list.entityType) &&
          (entity.role       == list.role      )
        )
      )

      setupDone = true
    }

  override protected def nonTerminalStores: Seq[EntityList] = lists

  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  override def content(collector: Collector): Caching.Parser[ScalaXml.Element] =
    for {_ <- setUp(collector)} yield {
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
}

object EntityLists extends Element[EntityLists]("entityLists") {

  override def contentParsable: Parsable[EntityLists] = new Parsable[EntityLists] {
    override def parser: Parser[EntityLists] = for {
      selector <- By.selectorParser
      lists <- EntityList.seq()
    } yield new EntityLists(
      selector,
      lists
    )

    override def unparser: Unparser[EntityLists] = Unparser.concat(
      By.selectorUnparser,
      EntityList.seq(_.lists)
    )
  }
}
