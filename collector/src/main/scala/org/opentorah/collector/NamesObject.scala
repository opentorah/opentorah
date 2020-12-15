package org.opentorah.collector

import org.opentorah.tei.{EntitiesList, EntityName, Ref, Tei}
import org.opentorah.util.Files
import scala.xml.{Elem, Node}

final class NamesObject(site: Site) extends SimpleSiteObject(site) {

  override def fileName: String = NamesObject.fileName

  override protected def viewer: Viewer = Viewer.Names

  override def title: Option[String] = Some(NamesObject.title)

  override protected def teiBody: Seq[Node] = {
    val nonEmptyLists: Seq[EntitiesList] = site.store.entities.get.lists.filterNot(_.isEmpty)
    val listOfLists: Seq[Node] =
      <p xmlns={Tei.namespace.uri}>{for (list <- nonEmptyLists) yield
        <l>{Ref.toXml(NamesObject.entityInTheListUrl(list.id), list.head)}</l>}</p>

    def toXml(value: EntitiesList): Elem =
      <list xmlns={Tei.namespace.uri} xml:id={value.id} role={value.role.orNull}>
        <head>{value.head}</head>
        {for (entity <- value.entities) yield <l>{EntityName.parsable.toXmlElement(entity.entityName)}</l>}
      </list>
        .copy(label = value.entityType.listElement)

    listOfLists ++ nonEmptyLists.flatMap(toXml)
  }

  def resolve(parts: Seq[String]): Option[SiteFile] =
    if (parts.isEmpty || parts.tail.nonEmpty) None else {
      val (fileName: String, extension: Option[String]) = Files.nameAndExtension(parts.head)
      site.findByRef(fileName).flatMap(entity => SimpleSiteObject.resolve(extension, new EntityObject(site, entity)))
    }

  override def simpleSubObjects: Seq[SimpleSiteObject] = Seq.empty
}

// TODO eliminate
object NamesObject {

  private val fileName: String = "names"

  val title: String = "Имена"

  def entityInTheListUrl(id: String): Seq[String] = Files.addPart(Seq(fileName + ".html"), id)
}
