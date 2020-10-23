package org.opentorah.collector

import org.opentorah.tei.{EntityName, Tei}
import org.opentorah.util.Files
import scala.xml.Node

final class MisnamedEntitiesReport(site: Site) extends ReportObject(site) {

  override protected def fileName: String = MisnamedEntitiesReport.fileName

  override protected def viewer: Viewer = Viewer.Names

  // TODO give a link to the entity:
  override protected def teiBody: Seq[Node] =
    <head xmlns={Tei.namespace.uri}>{MisnamedEntitiesReport.title}</head> ++
    site.entities.flatMap { entity =>
      val id: String = entity.id.get
      val expectedId: String = Files.spacesToUnderscores(entity.name)
      if (id == expectedId) None
      else Some(
        <l xmlns={Tei.namespace.uri}>
        {EntityName.toXmlElement(new EntityName(
          entityType = entity.entityType,
          ref = Some(id),
          name = id
        ))}
        {s"должен по идее называться '$expectedId'"}
      </l>)
    }
}

object MisnamedEntitiesReport {

  val fileName: String = "misnamed-entities"

  val title: String = "Неправильно названные файлы с именами"
}
