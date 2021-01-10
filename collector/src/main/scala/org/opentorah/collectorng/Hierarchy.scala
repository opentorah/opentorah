package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Body, Title}
import org.opentorah.xml.{Antiparser, Element, FromUrl, Parsable, Parser}

final class Hierarchy(
  override val fromUrl: FromUrl,
  override val names: Names,
  val title: Option[Title.Value],
  val storeAbstract: Option[Abstract.Value],
  val body: Option[Body.Value],
  val by: Option[ByHierarchy] // TODO make required (and deal with the Jerusalem archive)?
) extends Store with FromUrl.With {
  override def findByName(name: String): Option[Store] = Store.findByName(name, by.toSeq)
  def htmlTitel: Option[String] = title.map(_.xml2string)
}

object Hierarchy extends Element[Hierarchy]("store") {
  override def contentParsable: Parsable[Hierarchy] = new Parsable[Hierarchy] {
    override def parser: Parser[Hierarchy] = for {
      fromUrl <- Element.currentFromUrl
      names <- Names.withDefaultNameParsable()
      title <- Title.element.optional()
      storeAbstract <- Abstract.element.optional()
      body <- Body.element.optional()
      by <- ByHierarchy.followRedirects.optional()
    } yield new Hierarchy(
      fromUrl,
      names,
      title,
      storeAbstract,
      body,
      by
    )

    override def antiparser: Antiparser[Hierarchy] = Antiparser.concat(
      Names.withDefaultNameParsable(_.names),
      Title.element.optional(_.title),
      Abstract.element.optional(_.storeAbstract),
      Body.element.optional(_.body),
      ByHierarchy.optional(_.by)
    )
  }
}
