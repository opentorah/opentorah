package org.opentorah.collector

import org.opentorah.entity.{Entity, EntityReference}
import org.opentorah.store.{Binding, EntityHolder, Path, Store, WithPath}
import org.opentorah.tei.Ref
import org.opentorah.util.{Collections, Files}
import scala.xml.{Elem, Node}

final class EntityObject(site: Site, entity: Entity) extends SimpleSiteObject(site) {

  override protected def urlPrefix: Seq[String] = EntityObject.urlPrefix

  override protected def fileName: String = EntityObject.fileName(entity)

  override protected def teiWrapperViewer: Viewer = Viewer.Names

  override protected def teiBody: Seq[Node] = Seq(Entity.toXml(entity.copy(content = entity.content :+ mentions)))

  private def mentions: Elem = {

    def sources(references: Seq[WithPath[EntityReference]]): Seq[Elem] = {
      val result: Seq[Option[Elem]] =
      for (source <- Collections.removeConsecutiveDuplicates(references.map(_.path))) yield {
        val sourceStore: Store = source.last.store
        val url: Option[Seq[String]] = sourceStore match {
          case teiHolder: TeiHolder => Some(DocumentObject.documentUrl(
            WithPath(source.init.init, source.init.init.last.store.asInstanceOf[Collection]),
            Hierarchy.fileName(teiHolder)))
          case document: Document => Some(DocumentObject.documentUrl(
            WithPath(source.init, source.init.last.store.asInstanceOf[Collection]),
            Hierarchy.fileName(document)))
          case collection: Collection => None // TODO Some(collectionUrl(collection)) when grouping is adjusted?
          case _ => None
        }
        url.map(url => Ref.toXml(url, Hierarchy.storeName(sourceStore)))
      }

      result.flatten
    }

    val id: String = EntityObject.fileName(entity)

    val (fromEntities: Seq[WithPath[EntityReference]], notFromEntities: Seq[WithPath[EntityReference]]) =
      site.references
      .filter(_.value.ref.contains(id))
      .partition(_.path.last.store.isInstanceOf[EntityHolder])

    val bySource: Seq[(Path, Seq[WithPath[EntityReference]])] =
      notFromEntities
        .filter(reference => (reference.path.length >=3) && reference.path.init.init.last.store.isInstanceOf[Collection])
        .groupBy(reference => reference.path.init.init).toSeq.sortBy(_._1)(EntityObject.pathOrdering)

    <p rendition="mentions">
      {Ref.toXml(NamesObject.entityInTheListUrl(id), "[...]")}
      {if (fromEntities.isEmpty) Seq.empty else {
      <l>
        <emph>{NamesObject.title}:</emph>
        {
        val result = for (source <- Collections.removeConsecutiveDuplicates(fromEntities.map(_.path))) yield {
          val entityHolder: EntityHolder = source.last.store.asInstanceOf[EntityHolder]
          Ref.toXml(
            target = EntityObject.teiWrapperUrl(entityHolder.entity),
            text = Hierarchy.storeName(entityHolder)
          )
        }

        result.init.map(elem => <span>{elem},</span>) :+ result.last
        }
      </l>}}
      {for ((source, references) <- bySource)
      yield <l><emph>{Hierarchy.fullName(source)}:</emph>{sources(references)}</l>}
    </p>
  }
}

object EntityObject {

  val directoryName: String = "names"

  // TODO eliminate
  private val urlPrefix: Seq[String] = Seq(directoryName)

  private def fileName(entity: Entity): String = entity.id.get

  def teiWrapperUrl(entity: Entity): Seq[String] = urlPrefix :+ (fileName(entity) + ".html")

  def resolve(site: Site, parts: Seq[String]): Option[SiteFile] =
    if (parts.isEmpty || parts.tail.nonEmpty) None else {
      val (fileName: String, extension: Option[String]) = Files.nameAndExtension(parts.head)
      site.findByRef(fileName).flatMap(entity => SimpleSiteObject.resolve(extension, new EntityObject(site, entity)))
    }

  // TODO move into store
  val pathOrdering: Ordering[Path] = (x: Path, y: Path) => Ordering.Iterable[Binding]((x: Binding, y: Binding) => {
    val selectorCompare: Int = x.selector.names.name.toLowerCase compare y.selector.names.name.toLowerCase
    if (selectorCompare != 0) selectorCompare
    else x.store.names.name.toLowerCase compare y.store.names.name.toLowerCase
  }).compare(x.path, y.path)
}
