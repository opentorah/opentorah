package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.site.Store
import org.opentorah.tei.{Abstract, Body, Title}
import org.opentorah.xml.{Element, FromUrl, Parsable, Parser, Unparser, Xml}
import zio.ZIO

final class Hierarchy(
  override val fromUrl: FromUrl,
  override val names: Names,
  override val title: Title.Value,
  override val storeAbstract: Option[Abstract.Value],
  override val body: Option[Body.Value],
  val by: ByHierarchy
) extends Hierarchical with FromUrl.With {

  override def getBy: Option[ByHierarchy] = Some(by)

  override def acceptsIndexHtml: Boolean = true

  override def findByName(name: String): Parser[Option[Store]] = Store.findByName(name, Seq(by))

  override def path(site: Site): Store.Path = site.store2path(this)

  override protected def innerContent(site: Site): Parser[Xml.Element] = ZIO.succeed(by.oneLevelIndex(site))
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
