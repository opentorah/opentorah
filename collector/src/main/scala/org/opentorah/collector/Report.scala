package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.html.A
import org.opentorah.tei.{EntityReference, Unclear}
import org.opentorah.store.{Context, Path, Terminal, Viewer, WithSource}
import org.opentorah.util.Strings
import org.opentorah.xml.{Caching, ScalaXml}
import java.net.URI

// TODO Reports is ApparatusViewer, but Report - DefaultViewer (Hierarchy)?!
sealed abstract class Report[T](name: String, val title: String) extends Terminal, Viewer.Default:
  final override def names: Names = Names(name)
  final override def htmlHeadTitle: Option[String] = Some(title)
  final override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  final override def content(path: Path, context: Context): Caching.Parser[ScalaXml.Element] = for
    lines: Seq[T] <- lines(Collector.get(context))
    pathShortener: Path.Shortener <- context.pathShortener
  yield
    <div>{
      for line: T <- lines yield lineToXml(line, context, pathShortener)
    }</div>

  protected def lines(collector: Collector): Caching.Parser[Seq[T]]

  protected def lineToXml(line: T, context: Context, pathShortener: Path.Shortener): ScalaXml.Element

object Report:

  object NoRefs extends Report[WithSource[EntityReference]](
    "no-refs",
    "Имена без атрибута /ref/"
  ):
    override protected def lines(collector: Collector): Caching.Parser[Seq[WithSource[EntityReference]]] =
      collector.getNoRefs

    override protected def lineToXml(reference: WithSource[EntityReference], context: Context, pathShortener: Path.Shortener): ScalaXml.Element =
      val source: String = reference.source
      <l>{reference.value.name.toString} в {A(URI(source))(text = source)}</l>

  object Unclears extends Report[WithSource[Unclear.Value]](
    "unclears",
    "Неясности"
  ):
    override protected def lines(collector: Collector): Caching.Parser[Seq[WithSource[Unclear.Value]]] =
      collector.getUnclears

    override protected def lineToXml(unclear: WithSource[Unclear.Value], context: Context, pathShortener: Path.Shortener): ScalaXml.Element =
      val source: String = unclear.source
      <l>{unclear.value.content.toString} в {A(URI(source))(text = source)}</l>

  object MisnamedEntities extends Report[Entity](
    "misnamed-entities",
    "Неправильно названные файлы с именами"
  ):
    override protected def lines(collector: Collector): Caching.Parser[Seq[Entity]] =
      collector.entities.stores.map(_
        .filterNot((entity: Entity) => entity.id == getExpectedId(entity))
        .sortBy(_.name)
      )

    override protected def lineToXml(entity: Entity, context: Context, pathShortener: Path.Shortener): ScalaXml.Element =
      <l>{a(context.path(entity), pathShortener)(text = entity.id)} {s"должен по идее называться '${getExpectedId(entity)}'"}</l>

    private def getExpectedId(entity: Entity): String = Strings.spacesToUnderscores(entity.mainName)

  val reports: Seq[Report[?]] = Seq(Report.NoRefs, Report.MisnamedEntities, Report.Unclears)
