package org.opentorah.collector

import org.opentorah.entity.{Entity, EntityReference}
import org.opentorah.store.{EntityHolder, Store, WithPath}
import org.opentorah.tei.Tei
import org.opentorah.util.{Collections, Files}
import scala.xml.Elem

final class EntityObject(site: Site, entity: Entity) extends SiteObject(site) {
  override def viewer: String = NamesObject.namesViewer

  override protected def teiUrl: Seq[String] = EntityObject.teiUrl(entity)

  override protected def teiWrapperUrl: Seq[String] = EntityObject.teiWrapperUrl(entity)

  override protected def tei: Tei = Tei(Seq(Entity.toXml(entity.copy(content = entity.content :+ mentions))))

  private def mentions: Elem = {

    def sources(references: Seq[WithPath[EntityReference]]): Seq[Elem] = {
      val result: Seq[Option[Elem]] =
      for (source <- Collections.removeConsecutiveDuplicates(references.map(_.path))) yield {
        val sourceStore: Store = source.last.store
        val url: Option[Seq[String]] = sourceStore match {
          case teiHolder: TeiHolder => Some(DocumentObject.documentUrl(source.init.init.last.store, Site.fileName(teiHolder)))
          case document: Document => Some(DocumentObject.documentUrl(source.init.last.store, Site.fileName(document)))
          case collection: Collection => None // Some(collectionUrl(collection)) when grouping is adjusted?
          case _ => None
        }
        url.map(url => Site.ref(url, sourceStore.names.name))
      }

      result.flatten
    }

    val id: String = entity.id.get

    val (fromEntities: Seq[WithPath[EntityReference]], notFromNames: Seq[WithPath[EntityReference]]) =
      site.references
      .filter(_.value.ref.contains(id))
      .partition(_.path.last.store.isInstanceOf[EntityHolder])

    val bySource: Seq[(String, Seq[WithPath[EntityReference]])] =
      notFromNames
        .filter(reference => (reference.path.length >=3) && reference.path.init.init.last.store.isInstanceOf[Collection])
        .groupBy(Site.referenceCollectionName).toSeq.sortBy(_._1)

    <p rendition="mentions">
      {Site.ref(NamesObject.entityInTheListUrl(id), "[...]")}
      {if (fromEntities.isEmpty) Seq.empty else {
      <l>
        <emph>{NamesObject.namesHead}:</emph>
        {
        val result = for (source <- Collections.removeConsecutiveDuplicates(fromEntities.map(_.path))) yield {
          val entityHolder: EntityHolder = source.last.store.asInstanceOf[EntityHolder]
          Site.ref(
            url = EntityObject.teiWrapperUrl(entityHolder.entity),
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

  val namesDirectoryName: String = "names"

  def teiUrl(entity: Entity): Seq[String] = Seq(namesDirectoryName, entity.id.get + ".xml")

  def teiWrapperUrl(entity: Entity): Seq[String] = Seq(namesDirectoryName, entity.id.get + ".html")

  def resolve(site: Site, parts: Seq[String]): Option[SiteFile] =
    if (parts.isEmpty || parts.tail.nonEmpty) None else {
      val (fileName: String, extension: Option[String]) = Files.nameAndExtension(parts.head)
      site.findByRef(fileName).flatMap(entity => SiteFile.resolve(extension, new EntityObject(site, entity)))
    }
}
