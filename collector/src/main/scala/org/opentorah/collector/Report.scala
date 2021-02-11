package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.tei.EntityReference
import org.opentorah.util.Files
import org.opentorah.xml.{Html, Xml}

abstract class Report[T](val name: String, val title: String) extends Store with HtmlContent {
  final override def names: Names = Names(name)
  final override def htmlHeadTitle: Option[String] = Some(title)
  final override def htmlBodyTitle: Option[Xml.Nodes] = htmlHeadTitle.map(Xml.mkText)

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
    override protected def lines(site: Site): Seq[EntityReference] = site.getReferences.noRef

    override protected def lineToXml(reference: EntityReference, site: Site): Xml.Element = {
      val source: String = reference.sourceUrl.get
      <l>{Xml.text(reference.name)} в {Html.a(path = Files.splitUrl(source))(text = source)}</l>
    }
  }

  object MisnamedEntities extends Report[Entity](
    "misnamed-entities",
    "Неправильно названные файлы с именами"
  ) {
    override protected def lines(site: Site): Seq[Entity] = site.entities.directoryEntries
      .filterNot(entity => entity.id == getExpectedId(entity))
      .sortBy(_.name)

    override protected def lineToXml(entity: Entity, site: Site): Xml.Element =
      <l>{entity.a(site)(text = entity.id)} {s"должен по идее называться '${getExpectedId(entity)}'"}</l>

    private def getExpectedId(entity: Entity): String = Files.spacesToUnderscores(entity.mainName)
  }
}