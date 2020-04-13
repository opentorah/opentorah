package org.opentorah.collector

import org.opentorah.entity.{Entity, EntityReference}
import org.opentorah.store.{EntityHolder, Store, WithPath}
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
            Site.fileName(teiHolder)))
          case document: Document => Some(DocumentObject.documentUrl(
            WithPath(source.init, source.init.last.store.asInstanceOf[Collection]),
            Site.fileName(document)))
          case collection: Collection => None // Some(collectionUrl(collection)) when grouping is adjusted?
          case _ => None
        }
        url.map(url => Ref.toXml(url, sourceStore.names.name))
      }

      result.flatten
    }

    val id: String = EntityObject.fileName(entity)

    val (fromEntities: Seq[WithPath[EntityReference]], notFromNames: Seq[WithPath[EntityReference]]) =
      site.references
      .filter(_.value.ref.contains(id))
      .partition(_.path.last.store.isInstanceOf[EntityHolder])

    val bySource: Seq[(String, Seq[WithPath[EntityReference]])] =
      notFromNames
        .filter(reference => (reference.path.length >=3) && reference.path.init.init.last.store.isInstanceOf[Collection])
        .groupBy(EntityObject.referenceCollectionName).toSeq.sortBy(_._1)

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
            text = entityHolder.names.name
          )
        }

        result.init.map(elem => <span>{elem},</span>) :+ result.last
        }
      </l>}}
      {for ((source, references) <- bySource)
      yield <l><emph>{source}:</emph>{sources(references)}</l>}
    </p>
  }
}

object EntityObject {

  val directoryName: String = "names"

  private val urlPrefix: Seq[String] = Seq(directoryName)

  private def fileName(entity: Entity): String = entity.id.get

  def teiWrapperUrl(entity: Entity): Seq[String] = urlPrefix :+ (fileName(entity) + ".html")

  def resolve(site: Site, parts: Seq[String]): Option[SiteFile] =
    if (parts.isEmpty || parts.tail.nonEmpty) None else {
      val (fileName: String, extension: Option[String]) = Files.nameAndExtension(parts.head)
      site.findByRef(fileName).flatMap(entity => SimpleSiteObject.resolve(extension, new EntityObject(site, entity)))
    }

  def referenceCollectionName(reference: WithPath[EntityReference]): String =
    reference.path.init.init.last.store.names.name
}
