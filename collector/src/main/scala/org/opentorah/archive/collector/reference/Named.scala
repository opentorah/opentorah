package org.opentorah.archive.collector.reference

import org.opentorah.reference.{Entity, Name}
import org.opentorah.util.Collections
import scala.xml.{Elem, Node}

final class Named(
  val storeNamed: org.opentorah.reference.Named,
  container: Names,
  namedUrl: String => String,
  namedInTheListUrl: String => String
) extends ReferenceSource(container) {

  def entity: Entity = storeNamed.entity

  def id: String = storeNamed.id
  def role: Option[String] = storeNamed.role
  def names: Seq[Name] = storeNamed.names
  def content: Seq[Node] = storeNamed.content

  override val references: Seq[Reference] = bindReferences(content.flatMap(element =>
    org.opentorah.reference.Reference.all(element)))

  override def isNames: Boolean = true

  override def viewer: String = "namesViewer"

  override def name: String = names.head.name

  override def url: String = namedUrl(id)

  def toListXml: Elem =
    <l><ref target={url} role="namesViewer">{names.head.toXml}</ref></l>

  def toXml(references: Seq[Reference]): Elem = {
    def sources(references: Seq[Reference]): Seq[Elem] =
      for (ref <- Collections.removeConsecutiveDuplicates(references.map(_.source)))
      yield <ref target={ref.url} role={ref.viewer}>{ref.name}</ref>

    val usedBy: Seq[Reference] = references.filter(_.ref.contains(id))
    val fromNames: Seq[Reference] = usedBy.filter(_.source.isNames)
    val bySource: Seq[(String, Seq[Reference])] = usedBy.filterNot(_.source.isNames)
          .groupBy(_.source.collection.reference).toSeq.sortBy(_._1)

    // TODO calculate numbers correctly: dups in "Alexander I"...
    val numbers: Seq[(String, Int)] =
      usedBy.groupBy(_.name.map(_.text.trim).mkString(" ")).mapValues(_.length).toSeq.sortBy(_._2).reverse
    //      <p rendition="usage">
    //        {for ((name, number) <- numbers) yield <l>{s"$name ($number)"}</l>}
    //      </p>

    <named xml:id={id} role={role.orNull}>
      {for (name <- names) yield name.toXml}
      {content}
      <p rendition="mentions">
        <ref target={namedInTheListUrl(id)} role="namesViewer">[...]</ref>
        {if (fromNames.isEmpty) Seq.empty else
        <l>
          <emph>{fromNames.head.source.collection.reference}:</emph>
          {
          val result = sources(fromNames)
          result.init.map(elem => <span>{elem},</span>) :+ result.last
          }
        </l>}
        {for ((source, references) <- bySource) yield <l><emph>{source}:</emph>{sources(references)}</l>}
      </p>
    </named>
      .copy(label = entity.element)
  }
}

object Named {

  def toXml(value: Named): Elem =
    <named role={value.role.orNull}>
      {for (name <- value.names) yield name.toXml}
      {value.content}
    </named>
      .copy(label = value.entity.element)

  private def mentions(bySource: Seq[(String, Seq[Reference])]): Seq[Elem] = {
      {for ((source, references) <- bySource) yield
      <l>
        <emph>{source}:</emph>
        {Collections.removeConsecutiveDuplicates(references.map(_.source)).flatMap { ref =>
        // TODO add commas more intelligently :)
          val body = /*if (ref.name.contains(" ")) ref.name + "," else*/ ref.name
          <ref target={ref.url} role={ref.viewer}>{body}</ref>}}
      </l>}
  }
}
