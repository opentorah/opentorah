package org.digitaljudaica.archive.collector.reference

import org.digitaljudaica.archive.collector.{Errors, Layout, Tei}
import org.digitaljudaica.util.Collections
import org.digitaljudaica.metadata.Xml.Ops
import scala.xml.{Elem, Node, Text}

// TODO structure the TEI file better: the names, information, list reference, mentions...
final class Named(
  rawXml: Elem,
  fileName: String,
  container: Names,
  layout: Layout,
  errors: Errors
) extends ReferenceSource(container) {

  private val xml: Elem = if (rawXml.label != Tei.topElement) rawXml else new Tei(rawXml).body.elems.head

  val entity: Entity = Entity.forElement(xml.label).get

  private val idOption: Option[String] = xml.idOption
  if (idOption.isDefined && idOption.get != fileName) errors.error(s"Wrong id $id in file $fileName")
  def id: String = fileName

  val role: Option[String] = xml.attributeOption("role")

  private val (nameElements: Seq[Elem], tail: Seq[Elem]) = xml.elems.span(_.label == entity.nameElement)

  private val names: Seq[Name] = for (nameElement <- nameElements) yield new Name(entity, nameElement)
  if (names.isEmpty) errors.error(s"No names for $id")

  private val content: Seq[Elem] = tail.map(_.withoutNamespace)

  override val references: Seq[Reference] = content.flatMap(element => parseReferences(element))

  override def isNames: Boolean = true

  override def viewer: String = "namesViewer"

  override def name: String = names.head.name

  override def url: String = layout.namedUrl(id)

  def toListXml: Elem =
    <l><ref target={layout.namedUrl(id)} role="namesViewer">{names.head.toXml}</ref></l>

  def toXml(references: Seq[Reference]): Elem = {
    <named xml:id={id} role={role.orNull}>
      {for (name <- names) yield name.toXml}
      {content.filterNot(Named.isMentions) :+ Named.mentions(
        references.filter(_.ref.get == id),
        <ref target={layout.namedInTheListUrl(id)} role="namesViewer">[...]</ref>
      )}
    </named>
      .copy(label = entity.element)
  }
}

object Named {

  private def isMentions(element: Elem): Boolean =
    (element.label == "p") && element.attributeOption("rendition").contains("mentions")

  private def mentions(references: Seq[Reference], toList: Elem): Elem = {
    val fromNames: Seq[Reference] = references.filter(_.source.isNames)
    val bySource: Seq[(String, Seq[Reference])] =
      (if (fromNames.isEmpty) Seq.empty else Seq((fromNames.head.source.collection.reference, fromNames))) ++
        references.filterNot(_.source.isNames).groupBy(_.source.collection.reference).toSeq.sortBy(_._1)

    <p rendition="mentions">
      {toList}
      {for ((source, references) <- bySource) yield <l>{
        Seq[Node](Text(source + ": ")) ++
          (for (ref <- Collections.removeConsecutiveDuplicates(references.map(_.source)))
            yield <ref target={ref.url} role={ref.viewer}>{ref.name}</ref>)
        }</l>
      }</p>
  }
}
