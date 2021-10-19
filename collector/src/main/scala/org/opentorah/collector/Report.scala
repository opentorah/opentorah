package org.opentorah.collector

import org.opentorah.metadata.{Named, Names}
import org.opentorah.html
import org.opentorah.tei.{EntityReference, Unclear}
import org.opentorah.site.HtmlContent
import org.opentorah.store.{Path, Terminal, WithSource}
import org.opentorah.util.Strings
import org.opentorah.xml.{Caching, ScalaXml}
import java.net.URI

// Reports is ApparatusViewer, but Report - DefaultViewer (Hierarchy)?!
sealed abstract class Report[T](name: String, val title: String) extends Terminal, HtmlContent.DefaultViewer[Collector]:
  final override def names: Names = Names(name)
  final override def htmlHeadTitle: Option[String] = Some(title)
  final override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  final override def content(path: Path, collector: Collector): Caching.Parser[ScalaXml.Element] =
    lines(collector).map(lines => <div>{for line <- lines yield lineToXml(line, collector)}</div>)

  protected def lines(collector: Collector): Caching.Parser[Seq[T]]

  protected def lineToXml(line: T, collector: Collector): ScalaXml.Element

object Report:

  object NoRefs extends Report[WithSource[EntityReference]](
    "no-refs",
    "Имена без атрибута /ref/"
  ):
    override protected def lines(collector: Collector): Caching.Parser[Seq[WithSource[EntityReference]]] = collector.getReferences.map(_
      .filter(_.value.ref.isEmpty)
      .sortBy(reference => reference.value.name.toString.toLowerCase)
    )

    override protected def lineToXml(reference: WithSource[EntityReference], collector: Collector): ScalaXml.Element =
      val source: String = reference.source
      <l>{reference.value.name.toString} в {html.a(URI(source))(text = source)}</l>

  object Unclears extends Report[WithSource[Unclear.Value]](
    "unclears",
    "Неясности"
  ):
    override protected def lines(collector: Collector): Caching.Parser[Seq[WithSource[Unclear.Value]]] =
      collector.getUnclears.map(_.sortBy(_.source))

    override protected def lineToXml(unclear: WithSource[Unclear.Value], collector: Collector): ScalaXml.Element =
      val source: String = unclear.source
      <l>{unclear.value.content.toString} в {html.a(URI(source))(text = source)}</l>

  object MisnamedEntities extends Report[Entity](
    "misnamed-entities",
    "Неправильно названные файлы с именами"
  ):
    override protected def lines(collector: Collector): Caching.Parser[Seq[Entity]] = collector.entities.stores.map(_
      .filterNot(entity => entity.id == getExpectedId(entity))
      .sortBy(_.name)
    )

    override protected def lineToXml(entity: Entity, collector: Collector): ScalaXml.Element =
      <l>{collector.a(collector.entityPath(entity))(text = entity.id)} {s"должен по идее называться '${getExpectedId(entity)}'"}</l>

    private def getExpectedId(entity: Entity): String = Strings.spacesToUnderscores(entity.mainName)

  val reports: Seq[Report[?]] = Seq(Report.NoRefs, Report.MisnamedEntities, Report.Unclears)