package org.opentorah.collector

import org.opentorah.tei.{Entity, EntityName, Tei}
import org.opentorah.util.Files
import org.opentorah.xml.Xml

final class MisnamedEntitiesReport(site: Site) extends ReportObject[Entity](site) {

  override def fileName: String = "misnamed-entities"

  override protected def viewer: Viewer = Viewer.Names

  override def title: Option[String] = Some("Неправильно названные файлы с именами")

  override protected def lines: Seq[Entity] = site.entities
    .filterNot(entity => entity.id.contains(getExpectedId(entity)))

  override protected def lineToXml(entity: Entity): Xml.Element =
    <l xmlns={Tei.namespace.uri}>
      {EntityName.toXmlElement(EntityName.forEntity(entity))}
      {s"должен по идее называться '${getExpectedId(entity)}'"}
    </l>

  private def getExpectedId(entity: Entity): String = Files.spacesToUnderscores(entity.name)

  override def simpleSubObjects: Seq[SimpleSiteObject] = Seq.empty
}
