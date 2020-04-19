package org.opentorah.collector

import scala.xml.Node

final class NoRefsReport(site: Site) extends ReportObject(site) {

  override protected def fileName: String = NoRefsReport.fileName

  override protected def teiWrapperViewer: Viewer = Viewer.Names

  // TODO give a link to the ref:
  override protected def teiBody: Seq[Node] =
    <head>{NoRefsReport.title}</head> ++
      (for (reference <- site.references.filter(_.value.ref.isEmpty)) yield
        <l>{reference.value.name.map(_.text.trim).mkString(" ") + " в " +
          Hierarchy.referenceCollectionName(reference) + ":" +
          Hierarchy.storeName(reference.path.last.store)}</l>)
}

object NoRefsReport {

  val fileName: String = "no-refs"

  val title: String = "Имена без атрибута /ref/"
}
