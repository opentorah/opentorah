package org.digitaljudaica.archive.collector.reference

import java.io.File
import cats.implicits._
import org.digitaljudaica.archive.collector.{Errors, Layout}
import org.digitaljudaica.tei.{Entity, Name, Tei}
import org.digitaljudaica.util.Collections
import org.digitaljudaica.xml.{ContentType, From, Parser, Xml}
import org.digitaljudaica.xml.Ops._
import scala.xml.{Elem, Node, Text}

// TODO structure the TEI file better: the names, information, list reference, mentions...
final class Named private(
  val entity: Entity,
  fileName: String,
  container: Names,
  val role: Option[String],
  names: Seq[Name],
  content: Seq[Elem],
  layout: Layout,
  errors: Errors
) extends ReferenceSource(container) {

  def id: String = fileName

  if (names.isEmpty) errors.error(s"No names for $id")

  override val references: Seq[Reference] = bindReferences(content.flatMap(element =>
    org.digitaljudaica.tei.Reference.all(element)))

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
  def apply(
    directory: File,
    fileName: String,
    container: Names,
    layout: Layout,
    errors: Errors
  ): Named = From.file(directory, fileName).parseDo(ContentType.Elements, unwrapTei(parser(
    fileName,
    container,
    layout,
    errors
  )))

  private def parser(
    fileName: String,
    container: Names,
    layout: Layout,
    errors: Errors
  ): Parser[Named] = for {
    name <- Xml.name
    entity = Entity.forElement(name).get
    idOption <- Xml.attribute.optional.id
    _ = if (idOption.isDefined && idOption.get != fileName) errors.error(s"Wrong id ${idOption.get} in file $fileName")
    role <- Xml.attribute.optional("role")
    names <- Xml.element.all(entity.nameElement, ContentType.Text, Name.parser(entity))
    content <- Xml.allElements
  } yield new Named(
    entity,
    fileName,
    container,
    role,
    names,
    content = content.map(elem => org.digitaljudaica.xml.Ops.removeNamespace(elem)),
    layout,
    errors
  )

  private def unwrapTei(parser: Parser[Named]): Parser[Named] = for {
    name <- Xml.name
    result <- if (name == Tei.elementName) Tei.bodyParser(Xml.element.required(ContentType.Elements, parser)) else parser
  } yield result


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
