package org.opentorah.store

import java.net.URL
import org.opentorah.metadata.Names
import org.opentorah.tei.{EntitiesList, Entity}
import org.opentorah.xml.{Antiparser, Attribute, Parser}

final class Entities(
  inheritedSelectors: Seq[Selector],
  urls: Urls,
  element: Entities.Element
) extends Store(
  inheritedSelectors,
  urls
) {
  def selector: Selector = selectorByName(element.selector)

  override def names: Names = selector.names

  override val by: Option[By[EntityHolder]] = Some(By.fromElement[By[EntityHolder]](
    selectors,
    urls,
    element.by.asInstanceOf[By.Inline],
    creator = new Entities.EntitiesBy(_, _, _)
  ))

  val lists: Seq[EntitiesList] = element.lists.map(_.take(by.get.stores.map(_.entity)))

  def findByRef(ref: String): Option[Entity] = by.get.stores.find(_.entity.id.get == ref).map(_.entity)
}

object Entities {

  final case class Element(
    selector: String,
    by: By.Element,
    lists: Seq[EntitiesList]
  )

  object parsable extends org.opentorah.xml.Element.WithToXml[Element]("entities") {

    private val selectorAttribute: Attribute[String] = Attribute("selector")

    override protected def parser: Parser[Element] = for {
      selector <- selectorAttribute.required
      by <- By.parsable.required
      lists <- EntitiesList.all
    } yield Element(
      selector,
      by,
      lists
    )

    override protected val antiparser: Antiparser[Element] = Antiparser.concat(
      selectorAttribute.toXml.compose(_.selector),
      By.parsable.toXml.compose(_.by),
      EntitiesList.toXmlSeq.compose(_.lists)
    )
  }

  final class EntitiesBy(
    inheritedSelectors: Seq[Selector],
    urls: Urls,
    element: By.Inline
  ) extends By.FromElement[EntityHolder](inheritedSelectors, urls, element) {

    protected def storeCreator: Store.Creator[EntityHolder] =
      throw new IllegalArgumentException("Entities can not (yet?) be loaded inline.")

    override protected def loadFromDirectory(
      fileNames: Seq[String],
      fileInDirectory: String => URL
    ): Seq[Parser[EntityHolder]] = for (fileName <- fileNames) yield {
      val fromUrl: URL = fileInDirectory(fileName)
      parseWithKnownId(fromUrl, id = fileName)
        .map(entity => new EntityHolder(inheritedSelectors, Urls.fromUrl(fromUrl), entity))
    }
  }

  def parseWithKnownId(
    fromUrl: URL,
    id: String,
  ): Parser[Entity] = for {
    result <- Entity.parse(fromUrl)
    _ <- Parser.check(result.id.isEmpty || result.id.contains(id),
      s"Incorrect id: ${result.id.get} instead of $id")
  } yield result.copy(
    id = Some(id)
  )
}
