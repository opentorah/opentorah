package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.tei.{EntityName, EntityReference}
import org.opentorah.util.Files
import org.opentorah.xml.{Html, Xml}

abstract class Report[T](val name: String, val title: String) extends Store with HtmlContent {
  final override def names: Names = Names(name)
  final override def htmlHeadTitle: Option[String] = Some(title)
  final override def htmlBodyTitle: Option[Seq[Xml.Node]] = htmlHeadTitle.map(Xml.mkText)

  final override def path(site: Site): Store.Path = Seq(Reports, this)

  final override def content(site: Site): Xml.Element =
    <div>{lines(site).map(line => lineToXml(line, site))}</div>

  protected def lines(site: Site): Seq[T]

  protected def lineToXml(line: T, site: Site): Xml.Element
}

object Report {

  object NoRefs extends Report[EntityReference](
    "no-refs",
    "Имена без атрибута /ref/"
  ) {
    override def viewer: Viewer = Viewer.Names

    override protected def lines(site: Site): Seq[EntityReference] = site.getReferences.noRef

    override protected def lineToXml(line: EntityReference, site: Site): Xml.Element = {
      val reference: Xml.Element = EntityName.xmlElement(
        EntityName(
          entityType = line.entityType,
          name = line.text
        )
      )

      val source: String = line.sourceUrl.get
      val link: Xml.Element = Html.a(
        path = Files.splitUrl(source),
        target = Some(Viewer.Names.name)
      )(
        text = source
      )

      <l>{reference} в {link}</l>
    }
  }

  object MisnamedEntities extends Report[Entity](
    "misnamed-entities",
    "Неправильно названные файлы с именами"
  ) {
    override def viewer: Viewer = Viewer.Names

    override protected def lines(site: Site): Seq[Entity] = site.entities.entities
      .filterNot(entity => entity.id == getExpectedId(entity))
      .sortBy(_.name)

    override protected def lineToXml(entity: Entity, site: Site): Xml.Element =
      <l>{entity.a(site)(text = entity.mainName)} {s"должен по идее называться '${getExpectedId(entity)}'"}</l>

    private def getExpectedId(entity: Entity): String = Files.spacesToUnderscores(entity.mainName)
  }
}