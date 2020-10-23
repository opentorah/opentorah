package org.opentorah.collector

import org.opentorah.tei.{EntityName, Tei}
import scala.xml.Node

final class NoRefsReport(site: Site) extends ReportObject(site) {

  override protected def fileName: String = NoRefsReport.fileName

  override protected def viewer: Viewer = Viewer.Names

  // TODO give a link to the ref:
  override protected def teiBody: Seq[Node] =
    <head xmlns={Tei.namespace.uri}>{NoRefsReport.title}</head> ++
      (for (reference <- site.references.filter(_.value.ref.isEmpty)) yield {
        // TODO call one method to get the ref?
        val collectionName: String = Hierarchy.referenceCollectionName(reference)
        val documentName: String = Hierarchy.storeName(reference.path.last.store)
        val ref: String = s"/$collectionName/$documentName"

        <l xmlns={Tei.namespace.uri}>
          {EntityName.toXmlElement(new EntityName(
          entityType = reference.value.entityType,
          name = reference.value.name.map(_.text.trim).mkString(" ")
        ))} в {ref}
        </l>
      })
}

object NoRefsReport {

  val fileName: String = "no-refs"

  val title: String = "Имена без атрибута /ref/"
}
