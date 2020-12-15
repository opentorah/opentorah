package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.tei.{Abstract, Body, Title}
import org.opentorah.xml.{Antiparser, Element, FromUrl, Parser}

final class Collection(
  override val fromUrl: FromUrl,
  override val names: Names,
  override val title: Option[Title.Value],
  override val storeAbstract: Option[Abstract.Value],
  override val body: Option[Body.Value],
  val by: CollectionBy
) extends Store

object Collection extends Element[Collection]("store") {
  override def parser: Parser[Collection] = for {
    fromUrl <- currentFromUrl
    names <- Names.withDefaultNameParser
    title <- Title.parsable.optional
    storeAbstract <- Abstract.parsable.optional
    body <- Body.parsable.optional
    by <- CollectionBy.followRedirects.required
  } yield new Collection(
    fromUrl,
    names,
    title,
    storeAbstract,
    body,
    by
  )

  override def antiparser: Antiparser[Collection] = Antiparser.concat(
    Names.antiparser.compose(_.names),
    Title.parsable.toXmlOption.compose(_.title),
    Abstract.parsable.toXmlOption.compose(_.storeAbstract),
    Body.parsable.toXmlOption.compose(_.body),
    CollectionBy.toXml.compose(_.by)
  )
}
