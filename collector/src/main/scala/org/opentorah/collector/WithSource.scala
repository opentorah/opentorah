package org.opentorah.collector

import org.opentorah.tei.Tei
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Element, Elements, Parsable, Parser, Unparser, Xml}
import java.net.URL

final class WithSource[T](val source: String, val value: T)

object WithSource {

  final class Of[T](elements: Elements[T]) extends Element[WithSource[T]]("withSource") {
    private val valueElement: Elements.Required[T] = elements.required

    override def contentParsable: Parsable[WithSource[T]] = new Parsable[WithSource[T]] {
      override protected def parser: Parser[WithSource[T]] = for {
        source <- WithSource.sourceAttribute()
        value <- valueElement()
      } yield new WithSource[T](
        source,
        value
      )

      override def unparser: Unparser[WithSource[T]] = Unparser.concat[WithSource[T]](
        WithSource.sourceAttribute(_.source),
        valueElement(_.value)
      )
    }
  }

  private val sourceAttribute: Attribute.Required[String] = Attribute("sourceUrl").required

  def apply[T](
    url: URL,
    name: String,
    value: Elements[T]
  ): ListFile[WithSource[T], Seq[WithSource[T]]] = new ListFile[WithSource[T], Seq[WithSource[T]]](
    url,
    name,
    entry = new Of[T](value),
    wrapper = identity
  )

  def all[T](
    site: Site,
    finder: Xml.Nodes => Seq[T]
  ): Seq[WithSource[T]] = {
    def withSource(htmlContent: HtmlContent, nodes: Xml.Nodes): Seq[WithSource[T]] = {
      val source: String = Files.mkUrl(htmlContent.path(site).map(_.structureName))
      for (withoutSource <- finder(nodes)) yield new WithSource[T](source, withoutSource)
    }

    val fromEntities: Seq[Seq[WithSource[T]]] =
      for (entity <- site.entities.directoryEntries) yield withSource(
        entity,
        entity.teiEntity(site).content
      )

    val fromHierarchicals: Seq[Seq[WithSource[T]]] =
      for (hierarchical <- site.hierarchies ++ site.collections) yield withSource(
        hierarchical,
        Seq(Some(hierarchical.title), hierarchical.storeAbstract, hierarchical.body).flatten.flatMap(_.xml)
      )

    val fromDocuments: Seq[Seq[WithSource[T]]] =
      for {
        collection <- site.collections
        document <- collection.directoryEntries
        text = collection.textFacet.of(document)
      } yield withSource(
        text,
        Seq(Tei.xmlElement(text.getTei))
      )

    (fromEntities ++ fromHierarchicals ++ fromDocuments).flatten
  }

  def resolve[T](site: Site, withSources: Seq[WithSource[T]]): Seq[Store] =
    for (withSource <- withSources) yield site.resolve(withSource.source).get.last
}
