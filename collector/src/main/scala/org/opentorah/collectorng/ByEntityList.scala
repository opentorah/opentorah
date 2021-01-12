package org.opentorah.collectorng

import org.opentorah.xml.{Unparser, Element, Parsable, Parser, Xml}

// TODO:
//  lazy val lists: Seq[EntitiesList] = element.lists.map(_.take(by.get.stores.map(_.entity)))
//  def fileById(id: String): Option[EntityFile] = Some(new EntityFile(id))
final class ByEntityList(
  override val selector: Selector,
  val lists: Seq[EntityList]
) extends By with HtmlContent {
  override def findByName(name: String): Option[Store] = ???

  override def viewer: Html.Viewer = Html.Viewer.Names
  override def isWide: Boolean = false
  override def htmlTitle: Option[String] = selector.title
  override def navigationLinks: Seq[Html.NavigationLink] = Seq.empty
  override def lang: Option[String] = None
  override def content(site: Site): Xml.Element = ???
}

object ByEntityList extends Element[ByEntityList]("byEntityList") {

  override def contentParsable: Parsable[ByEntityList] = new Parsable[ByEntityList] {
    override def parser: Parser[ByEntityList] = for {
      selector <- By.selector
      lists <- EntityList.seq()
    } yield new ByEntityList(
      selector,
      lists
    )

    override def unparser: Unparser[ByEntityList] = Unparser.concat(
      By.selectorToXml,
      EntityList.seq(_.lists)
    )
  }
}
