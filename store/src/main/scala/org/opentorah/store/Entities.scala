package org.opentorah.store

import java.net.URL
import org.opentorah.metadata.Names
import org.opentorah.tei.{EntitiesList, Entity}
import org.opentorah.xml.{Unparser, Attribute, Parsable, Parser}

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

  // TODO write a list with id (file name), entity type and role;
  // add those to the EntityHolder;
  // make EntitiesList contain and findByRef return EntityHolder;
  // load Entity in the EntityHolder on demand.

  lazy val lists: Seq[EntitiesList] = element.lists.map(_.take(by.get.stores.map(_.entity)))

  def findByRef(ref: String): Option[Entity] = by.get.stores.find(_.entity.id.get == ref).map(_.entity)
}

object Entities {

  final case class Element(
    selector: String,
    by: By.Element,
    lists: Seq[EntitiesList]
  )

  object parsable extends org.opentorah.xml.Element[Element]("entities") {

    private val selectorAttribute: Attribute.Required[String] = Attribute("selector").required

    override def contentParsable: Parsable[Element] = new Parsable[Element] {
      override def parser: Parser[Element] = for {
        selector <- selectorAttribute()
        by <- By.parsable.required()
        lists <- EntitiesList.seq()
      } yield Element(
        selector,
        by,
        lists
      )

      override val unparser: Unparser[Element] = Unparser.concat(
        selectorAttribute(_.selector),
        By.parsable.required(_.by),
        EntitiesList.seq(_.lists)
      )
    }
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
