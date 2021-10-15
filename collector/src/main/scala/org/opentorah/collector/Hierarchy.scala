package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.store.Store
import org.opentorah.tei.{Abstract, Body, Title}
import org.opentorah.xml.{Element, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO

final class Hierarchy(
  override val fromUrl: Element.FromUrl,
  override val names: Names,
  override val title: Title.Value,
  override val storeAbstract: Option[Abstract.Value],
  override val body: Option[Body.Value],
  val by: ByHierarchy
) extends Hierarchical, Element.FromUrl.With:

  override def storesPure: Seq[ByHierarchy] = Seq(by)

  override def getBy: Option[ByHierarchy] = Some(by)

  override protected def innerContent(path: Store.Path, collector: Collector): Parser[ScalaXml.Element] =
    ZIO.succeed(by.oneLevelIndex(path, collector))

object Hierarchy extends Element[Hierarchy]("store"):

  override def contentParsable: Parsable[Hierarchy] = new Parsable[Hierarchy]:
    override def parser: Parser[Hierarchy] = for
      fromUrl: Element.FromUrl <- Element.fromUrl
      names: Names <- Names.withDefaultNameParsable()
      title: Title.Value <- Title.element.required()
      storeAbstract: Option[Abstract.Value] <- Abstract.element.optional()
      body: Option[Body.Value] <- Body.element.optional()
      by: ByHierarchy <- ByHierarchy.followRedirects.required()
    yield Hierarchy(
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
