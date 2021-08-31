package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.store.Store
import org.opentorah.tei.{Abstract, Body, Title}
import org.opentorah.xml.{Element, FromUrl, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO

final class Hierarchy(
  override val fromUrl: FromUrl,
  override val names: Names,
  override val title: Title.Value,
  override val storeAbstract: Option[Abstract.Value],
  override val body: Option[Body.Value],
  val by: ByHierarchy
) extends Hierarchical with FromUrl.With {

  override protected def nonTerminalStores: Seq[Store.NonTerminal] = Seq(by)

  override def getBy: Option[ByHierarchy] = Some(by)

  override protected def innerContent(collector: Collector): Parser[ScalaXml.Element] =
    ZIO.succeed(by.oneLevelIndex(collector))
}

object Hierarchy extends Element[Hierarchy]("store") {

  override def contentParsable: Parsable[Hierarchy] = new Parsable[Hierarchy] {
    override def parser: Parser[Hierarchy] = for {
      fromUrl <- Element.currentFromUrl
      names <- Names.withDefaultNameParsable()
      title <- Title.element.required()
      storeAbstract <- Abstract.element.optional()
      body <- Body.element.optional()
      by <- ByHierarchy.followRedirects.required()
    } yield new Hierarchy(
      fromUrl,
      names,
      title,
      storeAbstract,
      body,
      by
    )

    override def unparser: Unparser[Hierarchy] = Unparser.concat(
      Names.withDefaultNameParsable(_.names),
      Title.element.required(_.title),
      Abstract.element.optional(_.storeAbstract),
      Body.element.optional(_.body),
      ByHierarchy.required(_.by)
    )
  }
}
