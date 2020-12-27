package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Body, Title}
import org.opentorah.xml.{Antiparser, Element, FromUrl, Parser}

final class Hierarchy(
  override val fromUrl: FromUrl,
  override val names: Names,
  val title: Option[Title.Value],
  val storeAbstract: Option[Abstract.Value],
  val body: Option[Body.Value],
  val by: Option[ByHierarchy] // TODO make required (and deal with the Jerusalem archive)?
) extends Store with FromUrl.With {
  override def findByName(name: String): Option[Store] = Store.findByName(name, by.toSeq)
}

object Hierarchy extends Element[Hierarchy]("store") {
  override def parser: Parser[Hierarchy] = for {
    fromUrl <- currentFromUrl
    names <- Names.withDefaultNameParser
    title <- Title.parsable.optional
    storeAbstract <- Abstract.parsable.optional
    body <- Body.parsable.optional
    by <- ByHierarchy.followRedirects.optional
  } yield new Hierarchy(
    fromUrl,
    names,
    title,
    storeAbstract,
    body,
    by
  )

  override def antiparser: Antiparser[Hierarchy] = Antiparser.concat(
    Names.antiparser(_.names),
    Title.parsable.toXmlOption(_.title),
    Abstract.parsable.toXmlOption(_.storeAbstract),
    Body.parsable.toXmlOption(_.body),
    ByHierarchy.toXmlOption(_.by)
  )
}
