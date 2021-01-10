package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.xml.Xml

final class CollectionAlias(val collection: Collection) extends Store with HtmlContent {

  def alias: String = collection.alias.get

  override val names: Names = Names(alias)

  override def findByName(name: String): Option[Store] = collection.findByName(name)

  override def viewer: Html.Viewer = collection.viewer
  override def isWide: Boolean = collection.isWide
  override def htmlTitle: Option[String] = collection.htmlTitle
  override def navigationLinks: Seq[Html.NavigationLink] = collection.navigationLinks
  override def lang: Option[String] = collection.lang
  override def content(site: Site): Xml.Element = collection.content(site)
}
