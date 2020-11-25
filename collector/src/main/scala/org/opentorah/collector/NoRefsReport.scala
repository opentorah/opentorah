package org.opentorah.collector

import org.opentorah.tei.{EntityName, Ref, Tei}
import scala.xml.Elem

final class NoRefsReport(site: Site) extends ReportObject[ReferenceWithSource.FromDocument](site) {

  override def fileName: String = "no-refs"

  override protected def viewer: Viewer = Viewer.Names

  override def title: Option[String] = Some("Имена без атрибута /ref/")

  override protected def lines: Seq[ReferenceWithSource.FromDocument] = site.references.noRef

  // TODO include all references, not just the ones from documents!
  override protected def lineToXml(fromDocument: ReferenceWithSource.FromDocument): Elem = {
    val reference: Elem = EntityName.toXmlElement(EntityName.forReference(fromDocument.reference))

    val link: Elem = Ref.toXml(
      target = fromDocument.shortPath,
      text = fromDocument.collectionFileName + "/" + fromDocument.documentName
    )

    <l xmlns={Tei.namespace.uri}>{reference} в {link}</l>
  }

  override def simpleSubObjects: Seq[SimpleSiteObject] = Seq.empty
}
