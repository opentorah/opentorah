package org.opentorah.collector

import org.opentorah.html.A
import org.opentorah.tei.Tei
import org.opentorah.store.{By, Context, Path, Pure}
import org.opentorah.xml.{Atom, Element, ElementTo, Elements, Nodes, Parsable, Parser, Unparser}
import zio.ZIO

final class EntityLists(
  selectorName: String,
  val lists: Seq[EntityList]
) extends
  By.WithSelector[EntityList](selectorName),
  Pure[EntityList]:
  
  def setUp(collector: Collector): Parser[Unit] = for
    entities: Seq[Entity] <- collector.entities.stores
  yield
    for list: EntityList <- lists do list.setEntities(
      entities.filter(entity =>
        (entity.entityType == list.entityType) &&
        (entity.role       == list.role      )
      )
    )

  override def storesPure: Seq[EntityList] = lists

  override def htmlHeadTitle: Option[String] = selector.title
  override def htmlBodyTitle: Option[Nodes] = htmlHeadTitle.map(Atom.apply)

  override def content(path: Path, context: Context): Parser[Element] =
    val nonEmptyLists: Seq[EntityList] = lists.filter(_.getEntities.nonEmpty)
    for
      tocA: A <- context.a(path)
      toc: Elements <- ZIO.foreach(nonEmptyLists)((list: EntityList) =>
        for contentA: A <- context.a(list) yield
          <l>
            {tocA.setFragment(list.names.name)(xml = list.title)}
            {contentA(EntityLists.expand)}
          </l>
      )
      content: Elements <- ZIO.foreach(nonEmptyLists)((list: EntityList) =>
        for
          a: A <- context.a(list)
          lines: Elements <- ZIO.foreach(list.getEntities)((entity: Entity) =>
            entity.line(context)
          )
        yield
          <list id={list.names.name}>
            <head xmlns={Tei.namespace.uri}>
              {list.title.content}
              {a(EntityLists.expand)}
            </head>
            {lines}
          </list>
      )
    yield
      <div>
        <p>{toc}</p>
        {content}
      </div>


object EntityLists extends ElementTo[EntityLists]("entityLists"):
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
