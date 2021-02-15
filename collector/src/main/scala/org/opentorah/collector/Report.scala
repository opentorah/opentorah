package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.tei.{EntityReference, Unclear}
import org.opentorah.util.Files
import org.opentorah.xml.{Html, Xml}
import java.net.URI

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

  object NoRefs extends Report[WithSource[EntityReference]](
    "no-refs",
    "Имена без атрибута /ref/"
  ) {
    override protected def lines(site: Site): Seq[WithSource[EntityReference]] =  site.getReferences
      .filter(_.value.ref.isEmpty)
      .sortBy(reference => Xml.text(reference.value.name).toLowerCase)

    override protected def lineToXml(reference: WithSource[EntityReference], site: Site): Xml.Element = {
      val source: String = reference.source
      <l>{Xml.text(reference.value.name)} в {Html.a.uri(new URI(source))(text = source)}</l>
    }
  }

  object Unclears extends Report[WithSource[Unclear.Value]](
    "unclears",
    "Неясности"
  ) {
    override protected def lines(site: Site): Seq[WithSource[Unclear.Value]] =
      site.getUnclears.sortBy(_.source)

    override protected def lineToXml(unclear: WithSource[Unclear.Value], site: Site): Xml.Element = {
      val source: String = unclear.source
      <l>{Xml.text(unclear.value.xml)} в {Html.a.uri(new URI(source))(text = source)}</l>
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