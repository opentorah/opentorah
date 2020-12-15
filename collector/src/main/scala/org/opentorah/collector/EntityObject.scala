package org.opentorah.collector

import org.opentorah.tei.{Entity, Ref, Tei}
import org.opentorah.util.Collections
import scala.xml.{Elem, Node}

final class EntityObject(site: Site, entity: Entity) extends SimpleSiteObject(site) {

  override protected def urlPrefix: Seq[String] = EntityObject.urlPrefix

  override def fileName: String = EntityObject.fileName(entity)

  override protected def viewer: Viewer = Viewer.Names

  override protected def teiBody: Seq[Node] = Seq(Entity.parsable.toXmlElement(entity.copy(content = entity.content :+ mentions)))

  private def mentions: Elem = {

    // TODO handle references from Collection, hierarchy etc.
    def sources(references: Seq[ReferenceWithSource]): Seq[Elem] = {
      val result: Seq[Option[Elem]] =
        for (reference <- Collections.removeConsecutiveDuplicatesWith(references)(_.path)) yield reference match {
          case fromDocument: ReferenceWithSource.FromDocument =>
            Some(Ref.toXml(
              fromDocument.shortPath,
              fromDocument.documentName
            ))

          case _ =>
            None
        }

      result.flatten
    }

    val id: String = EntityObject.fileName(entity)

    val (fromEntities: Seq[ReferenceWithSource.FromEntity], fromDocuments: Seq[ReferenceWithSource.FromDocument]) =
      site.references.toId(id)

    <p xmlns={Tei.namespace.uri} rendition="mentions">
      {Ref.toXml(NamesObject.entityInTheListUrl(id), "[...]")}
      {if (fromEntities.isEmpty) Seq.empty else {
      <l>
        <emph>{NamesObject.title}:</emph>
        {
        val result = for (fromEntity <- Collections.removeConsecutiveDuplicatesWith(fromEntities)(_.path)) yield Ref.toXml(
          target = fromEntity.path,
          text = fromEntity.entityName
        )

        result.init.map(elem => <span>{elem},</span>) :+ result.last
        }
      </l>}}
      {for (references <- fromDocuments.groupBy(_.path.init).toSeq.sortBy(_._1)(Hierarchy.pathOrdering).map(_._2))
      yield <l><emph>{Hierarchy.fullName(references.head.path.init.init)}:</emph>{sources(references)}</l>}
    </p>
  }

  override def simpleSubObjects: Seq[SimpleSiteObject] = Seq.empty
}

object EntityObject {

  val directoryName: String = "names"

  // TODO eliminate
  private val urlPrefix: Seq[String] = Seq(directoryName)

  private def fileName(entity: Entity): String = entity.id.get

  // TODO eliminate
  def teiWrapperUrl(entity: Entity): Seq[String] = urlPrefix :+ (fileName(entity) + ".html")
}
