package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.store.{Context, Path}
import org.opentorah.tei.{Abstract, Body, Title}
import org.opentorah.xml.{Caching, Element, Parsable, Parser, ScalaXml, Unparser}
import zio.ZIO

final class Hierarchy(
  fromUrl: Element.FromUrl,
  names: Names,
  title: Title.Value,
  description: Option[Abstract.Value],
  body: Option[Body.Value],
  val by: ByHierarchy
) extends Hierarchical(
  fromUrl,
  names,
  title,
  description,
  body
):
  override def storesPure: Seq[ByHierarchy] = Seq(by)

  override def getBy: Option[ByHierarchy] = Some(by)

  override def content(path: Path, context: Context): Caching.Parser[ScalaXml.Element] = for
    pathShortener: Path.Shortener <- context.pathShortener
  yield by.oneLevelIndex(path :+ by, pathShortener)

object Hierarchy extends Element[Hierarchy]("store"):
  override def contentParsable: Parsable[Hierarchy] = new Parsable[Hierarchy]:
    override def parser: Parser[Hierarchy] = for
      fromUrl: Element.FromUrl <- Element.fromUrl
      names: Names <- Hierarchical.namesParsable()
      title: Title.Value <- Hierarchical.titleElement()
      description: Option[Abstract.Value] <- Hierarchical.descriptionElement()
      body: Option[Body.Value] <- Hierarchical.bodyElement()
      by: ByHierarchy <- ByHierarchy.followRedirects.required()
    yield Hierarchy(
      fromUrl,
      names,
      title,
      description,
      body,
      by
    )

    override def unparser: Unparser[Hierarchy] = Unparser.concat(
      Names.withDefaultNameParsable(_.names),
      Title.element.required(_.title),
      Abstract.element.optional(_.description),
      Body.element.optional(_.body),
      ByHierarchy.required(_.by)
    )
