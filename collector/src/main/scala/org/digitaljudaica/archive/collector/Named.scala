package org.digitaljudaica.archive.collector

import java.io.File

import scala.xml.{Elem, Node, Text}
import Xml.Ops

final case class Named(
  layout: Layout,
  namesContainer: Names,
  id: String,
  role: Option[String],
  names: Seq[Name],
  content: Seq[Elem],
  entity: Entity,
  errors: Errors
) extends HasReferences {
  if (names.isEmpty) errors.error(s"No names for $id")

  override val references: Seq[Reference] = content.flatMap(element => Reference.parseReferences(this, element, errors))

  override def isNames: Boolean = true

  override def collectionReference: String = namesContainer.head

  override def viewer: String = "namesViewer"

  override def name: String = names.head.name

  override def url: String = layout.namedUrl(id)

  def addMentions(references: Seq[Reference]): Named = {
    def isMentions(element: Elem): Boolean =
      (element.label == "p") && element.attributeOption("rendition").contains("mentions")

    copy(content = content.filterNot(isMentions) :+ mentions(references.filter(_.ref.get == id)))
  }

  private def mentions(references: Seq[Reference]): Elem = {
    val fromNames: Seq[Reference] = references.filter(_.source.isNames)
    val bySource: Seq[(String, Seq[Reference])] =
      (if (fromNames.isEmpty) Seq.empty else Seq((fromNames.head.source.collectionReference, fromNames))) ++
        references.filterNot(_.source.isNames).groupBy(_.source.collectionReference).toSeq.sortBy(_._1)

    <p rendition="mentions">{
      for ((source, references) <- bySource) yield <l>{
        Seq[Node](Text(source + ": ")) ++
          (for (ref <- Util.removeConsecutiveDuplicates(references.map(_.source)))
           yield <ref target={ref.url} role={ref.viewer}>{ref.name}</ref>)
        }</l>
    }</p>
  }

  def toXml: Elem =
    <named xml:id={id} role={role.orNull}>
      {for (name <- names) yield name.toXml}
      {content}
    </named>
      .copy(label = entity.element)
}

object Named {

  final def parse(
    layout: Layout,
    namesContainer: Names,
    directory: File,
    errors: Errors
  ): Seq[Named] = {
    for (fileName <- Util.filesWithExtensions(directory, ".xml").sorted)
      yield parse(layout, namesContainer, directory, fileName, errors)
  }

  private def parse(
    layout: Layout,
    namesContainer: Names,
    listDirectory: File,
    fileName: String,
    errors: Errors
  ): Named = {
    val xml: Elem = Xml.load(listDirectory, fileName)
    val entity: Entity = Entity.forElement(xml.label).get
    val id: Option[String] = xml.attributeOption("xml:id")
    if (id.isDefined && id.get != fileName) errors.error(s"Wrong id $id in file $fileName")

    val (nameElements: Seq[Elem], tail: Seq[Elem]) = xml.elements.span(_.label == entity.nameElement)
    Named(
      layout,
      namesContainer,
      id = fileName,
      role = xml.attributeOption("role"),
      names = Name.parseNames(entity, nameElements, errors),
      content = tail.map(_.withoutNamespace),
      entity,
      errors
    )
  }
}
