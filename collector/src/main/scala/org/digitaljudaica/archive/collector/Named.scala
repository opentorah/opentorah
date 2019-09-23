package org.digitaljudaica.archive.collector

import java.io.File

import scala.xml.Elem
import Xml.Ops

final case class Named(
  document: DocumentLike,
  id: String,
  names: Seq[Name],
  content: Seq[Elem],
  entity: Entity,
  errors: Errors
) {
  if (names.isEmpty) errors.error(s"No names for $id")

  val references: Seq[Reference] = content.flatMap(element => Reference.parseReferences(document, element, errors))

  def isMentions(element: Elem): Boolean =
    element.label == "p" && (element \ "@rendition").text == "mentions"

  def addMentions(references: Seq[Reference]): Named = {
    val (nonMentions: Seq[Elem], tail: Seq[Elem]) = content.span(element => !isMentions(element))

    val (before: Seq[Elem], after: Seq[Elem]) = if (tail.nonEmpty) (nonMentions, tail.tail) else (Nil, content)

    copy(content = before ++ Seq(mentions(references)) ++ after)
  }

  private def mentions(references: Seq[Reference]): Elem = {
    <p rendition="mentions">
      {for (ref <- Util.removeConsecutiveDuplicates(references.filter(_.ref.get == id).map(_.document)))
      yield <ref target={ref.url} role="documentViewer">{ref.name}</ref>}
    </p>
  }

  def toXml: Elem =
    <named xml:id={id}>
      {for (name <- names) yield name.toXml}
      {content}
    </named>
      .copy(label = entity.element)
}

object Named {
  final def parse(entity: Entity, document: DocumentLike, listDirectory: File, fileName: String, errors: Errors): Named = {
    val xml: Elem = Xml.load(listDirectory, fileName)
    xml.check(entity.element)
    val id: Option[String] = xml.attributeOption("xml:id")
    if (id.isDefined && id.get != fileName) errors.error(s"Wrong id $id in file $fileName")

    val (nameElements: Seq[Elem], tail: Seq[Elem]) = xml.elements.span(_.label == entity.nameElement)
    Named(
      document,
      id = fileName,
      names = Name.parseNames(entity, nameElements, errors),
      content = tail.map(Xml.removeNamespace),
      entity,
      errors
    )
  }
}
