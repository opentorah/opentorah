package org.opentorah.collector

import org.opentorah.site.{Caching, ListFile}
import org.opentorah.tei.Tei
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Element, Elements, Parsable, Parser, Unparser, Xml}
import zio.ZIO
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
    finder: Xml.Nodes => Parser[Seq[T]]
  ): Caching.Parser[Seq[WithSource[T]]] = {

    def withSource(htmlContent: HtmlContent, nodes: Xml.Nodes): Parser[Seq[WithSource[T]]] = {
      val source: String = Files.mkUrl(htmlContent.path(site).map(_.structureName))
      finder(nodes).map(_.map(new WithSource[T](source, _)))
    }

    for {
      entities <- site.entities.directoryEntries
      fromEntities <- ZIO.foreach(entities)(entity =>
          entity.teiEntity(site) >>= (teiEntity => withSource(entity, teiEntity.content)))
      fromHierarchicals <- ZIO.foreach(site.hierarchies ++ site.collections) { hierarchical => withSource(
        hierarchical,
        Seq(Some(hierarchical.title), hierarchical.storeAbstract, hierarchical.body).flatten.flatMap(_.xml)
      )}
      fromDocuments <- ZIO.foreach(site.collections) { collection =>
        collection.directoryEntries >>= (documents => ZIO.foreach(documents) { document =>
          val text: Document.TextFacet = collection.textFacet.of(document)
          text.getTei >>= (tei => withSource(text, Seq(Tei.xmlElement(tei))))
        })
      }
    } yield (fromEntities ++ fromHierarchicals ++ fromDocuments.flatten).flatten
  }
}
