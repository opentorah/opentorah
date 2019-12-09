package org.digitaljudaica.archive.collector

import java.io.File

import scala.xml.{Elem, Node, Text}
import Xml.Ops

// TODO structure the TEI file better: the names, information, list reference, mentions...
final case class Named(
  container: Names,
  layout: Layout,
  id: String,
  role: Option[String],
  names: Seq[Named.Name],
  content: Seq[Elem],
  entity: Entity
) extends DocumentLike(container) {

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

  final case class Name(
    name: String,
    id: Option[String],
    entity: Entity
  ) {
    def toXml: Elem = {
      <name xml:id={id.orNull}>{name}</name>
        .copy(label = entity.nameElement)
    }
  }

  object Name {

    def apply(entity: Entity, xml: Elem): Name = {
      xml.check(entity.nameElement)

      Name(
        name = xml.text,
        id = xml.attributeOption("xml:id"),
        entity
      )
    }
  }

  final def parse(
    layout: Layout,
    namesContainer: Names,
    directory: File,
    errors: Errors
  ): Seq[Named] = {
    def parse(fileName: String): Named = {
      val rawXml: Elem = Xml.load(directory, fileName)
      val xml: Elem = if (rawXml.label != Tei.topElement) rawXml else new Tei(rawXml).body.elements.head
      val entity: Entity = Entity.forElement(xml.label).get
      val id: Option[String] = xml.idOption
      if (id.isDefined && id.get != fileName) errors.error(s"Wrong id $id in file $fileName")

      val (nameElements: Seq[Elem], tail: Seq[Elem]) = xml.elements.span(_.label == entity.nameElement)
      val names: Seq[Name] = for (nameElement <- nameElements) yield Name(entity, nameElement)
      if (names.isEmpty) errors.error(s"No names for $id")

      Named(
        namesContainer,
        layout,
        id = fileName,
        role = xml.attributeOption("role"),
        names,
        content = tail.map(_.withoutNamespace),
        entity
      )
    }

    for (fileName <- Util.filesWithExtensions(directory, extension = ".xml").sorted) yield parse(fileName)
  }

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
          (for (ref <- Util.removeConsecutiveDuplicates(references.map(_.source)))
            yield <ref target={ref.url} role={ref.viewer}>{ref.name}</ref>)
        }</l>
      }</p>
  }
}
