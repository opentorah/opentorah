package org.opentorah.collector

import org.opentorah.entity.{EntitiesList, EntityName}
import scala.xml.{Elem, Node}

final class NamesObject(site: Site) extends SiteObject(site) {
  override def viewer: String = NamesObject.namesViewer

  override def teiFile: TeiFile = new TeiFile(this) {
    override def url: Seq[String] = Seq(NamesObject.namesFileName + ".xml")

    override protected def xml: Seq[Node] = {
      val nonEmptyLists: Seq[EntitiesList] = site.entitiesLists.filterNot(_.isEmpty)
      val listOfLists: Seq[Node] =
        <p>{for (list <- nonEmptyLists) yield <l>{Site.ref(NamesObject.entityInTheListUrl(list.id), list.head)}</l>}</p>

      def toXml(value: EntitiesList): Elem =
        <list xml:id={value.id} role={value.role.orNull}>
          <head>{value.head}</head>
          {for (entity <- value.entities) yield {
          <l>{Site.refNg(EntityObject.entityUrl(entity), EntityName.toXml(entity.names.head))}</l>
        }}
        </list>
          .copy(label = value.entityType.listElement)

      <head>{NamesObject.namesHead}</head> ++ listOfLists ++ nonEmptyLists.flatMap(toXml)
    }
  }

  override def teiWrapperFile: TeiWrapperFile = new TeiWrapperFile(this) {
    override def url: Seq[String] = Seq(NamesObject.namesFileName + ".html")

    override def yaml: Seq[(String, String)] = Seq("title" -> NamesObject.namesHead)
  }
}

object NamesObject {

  val namesFileName: String = "names"

  val namesHead: String = "Имена"

  val namesViewer: String = "namesViewer"

  // TODO do Seq()
  def entityInTheListUrl(id: String): String = s"/$namesFileName.html#$id"

  def resolve(site: Site, extension: Option[String], parts: Seq[String]): Option[SiteFile] =
    if (parts.nonEmpty) None
    else SiteFile.resolve(extension, new NamesObject(site))
}
