package org.opentorah.collectorng

import org.opentorah.xml.{Antiparser, Element, Parser}

// TODO:
//  lazy val lists: Seq[EntitiesList] = element.lists.map(_.take(by.get.stores.map(_.entity)))
//  def fileById(id: String): Option[EntityFile] = Some(new EntityFile(id))
final class ByEntityList(
  override val selector: Selector,
  val lists: Seq[EntityList]
) extends By {
  override def findByName(name: String): Option[Store] = ???
}

object ByEntityList extends Element[ByEntityList]("byEntityList") {

  override def parser: Parser[ByEntityList] = for {
    selector <- By.selector
    lists <- EntityList.all
  } yield new ByEntityList(
    selector,
    lists
  )

  override def antiparser: Antiparser[ByEntityList] = Antiparser.concat(
    By.selectorToXml,
    EntityList.toXmlSeq(_.lists)
  )
}
