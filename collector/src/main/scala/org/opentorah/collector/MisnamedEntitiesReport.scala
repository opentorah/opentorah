package org.opentorah.collector

import org.opentorah.tei.{Entity, EntityName, Tei}
import org.opentorah.util.Files
import scala.xml.Elem

final class MisnamedEntitiesReport(site: Site) extends ReportObject[Entity](site) {

  override protected def fileName: String = MisnamedEntitiesReport.fileName

  override protected def viewer: Viewer = Viewer.Names

  override def title: Option[String] = Some(MisnamedEntitiesReport.title)

  override protected def lines: Seq[Entity] = site.entities
    .filterNot(entity => entity.id.contains(getExpectedId(entity)))

  override protected def lineToXml(entity: Entity): Elem =
    <l xmlns={Tei.namespace.uri}>
      {EntityName.toXmlElement(EntityName.forEntity(entity))}
      {s"должен по идее называться '${getExpectedId(entity)}'"}
    </l>

  private def getExpectedId(entity: Entity): String = Files.spacesToUnderscores(entity.name)
}

object MisnamedEntitiesReport {

  val fileName: String = "misnamed-entities"

  val title: String = "Неправильно названные файлы с именами"
}
