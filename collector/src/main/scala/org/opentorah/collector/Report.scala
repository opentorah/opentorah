package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.tei.{EntityReference, Unclear}
import org.opentorah.store.{Context, Path, Terminal, WithSource}
import org.opentorah.util.Strings
import org.opentorah.xml.{A, Caching, ScalaXml}
import zio.ZIO

import java.net.URI

// TODO Reports is ApparatusViewer, but Report - DefaultViewer (Hierarchy)?!
sealed abstract class Report[T](name: String, val title: String) extends Terminal:
  final override def names: Names = Names(name)
  final override def htmlHeadTitle: Option[String] = Some(title)
  final override def htmlBodyTitle: Option[ScalaXml.Nodes] = htmlHeadTitle.map(ScalaXml.mkText)

  final override def content(path: Path, context: Context): Caching.Parser[ScalaXml.Element] = for
    lines: Seq[T] <- lines(Collector.get(context))
    result: Seq[ScalaXml.Element] <- ZIO.foreach(lines)((line: T) => 
      lineToXml(line, context)
    )
  yield
    <div>{result}</div>

  protected def lines(collector: Collector): Caching.Parser[Seq[T]]

  protected def lineToXml(line: T, context: Context): Caching.Parser[ScalaXml.Element]

object Report:

  private object NoRefs extends Report[WithSource[EntityReference]](
    name = "no-refs",
    title = "Имена без атрибута /ref/"
  ):
    override protected def lines(collector: Collector): Caching.Parser[Seq[WithSource[EntityReference]]] =
      collector.getNoRefs

    override protected def lineToXml(reference: WithSource[EntityReference], context: Context): Caching.Parser[ScalaXml.Element] =
      val source: String = reference.source
      ZIO.succeed(<l>{reference.value.name.toString} в {A(URI(source))(text = source)}</l>)

  private object Unclears extends Report[WithSource[Unclear.Value]](
    name = "unclears",
    title = "Неясности"
  ):
    override protected def lines(collector: Collector): Caching.Parser[Seq[WithSource[Unclear.Value]]] =
      collector.getUnclears

    override protected def lineToXml(unclear: WithSource[Unclear.Value], context: Context): Caching.Parser[ScalaXml.Element] =
      val source: String = unclear.source
      ZIO.succeed(<l>{unclear.value.content.toString} в {A(URI(source))(text = source)}</l>)

  private object MisnamedEntities extends Report[Entity](
    name = "misnamed-entities",
    title = "Неправильно названные файлы с именами"
  ):
    override protected def lines(collector: Collector): Caching.Parser[Seq[Entity]] =
      collector.entities.stores.map(_
        .filterNot((entity: Entity) => entity.id == getExpectedId(entity))
        .sortBy(_.name)
      )

    override protected def lineToXml(entity: Entity, context: Context): Caching.Parser[ScalaXml.Element] =
      for a: A <- context.a(entity)
      yield <l>{a(text = entity.id)} {s"должен по идее называться '${getExpectedId(entity)}'"}</l>

    private def getExpectedId(entity: Entity): String = Strings.spacesToUnderscores(entity.mainName)

  val reports: Seq[Report[?]] = Seq(Report.NoRefs, Report.MisnamedEntities, Report.Unclears)
