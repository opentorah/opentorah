package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.tei.Title
import org.opentorah.xml.{Antiparser, Element, FromUrl, Parser}

final class RootStore(
  override val fromUrl: FromUrl,
  val names: Names,
  val title: Option[Title.Value],
  val entities: Entities,
  val by: HierarchyBy
) extends FromUrl.With {

  private val indexFile = new IndexFile

  def resolve(url: Seq[String]): Option[SiteFile] =
    if (url.isEmpty || (url.length == 1 && url.head == indexFile.name)) Some(indexFile) else
    if (url.length == 2 && entities.selector.resolves(url)) entities.fileById(url(1)) else
      // TODO notes
      by.resolve(url)
}

object RootStore extends Element[RootStore]("rootStore") {
  override def parser: Parser[RootStore] = for {
    fromUrl <- currentFromUrl
    names <- Names.withDefaultNameParser
    title <- Title.parsable.optional
    entities <- Entities.required
    by <- HierarchyBy.followRedirects.required
  } yield new RootStore(
    fromUrl,
    names,
    title,
    entities,
    by
  )

  override def antiparser: Antiparser[RootStore] = Antiparser.concat[RootStore](
    Names.antiparser.compose(_.names),
    Title.parsable.toXmlOption.compose(_.title),
    Entities.toXml.compose(_.entities),
    HierarchyBy.toXml.compose(_.by)
  )
}
