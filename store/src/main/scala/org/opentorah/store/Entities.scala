package org.opentorah.store

import java.net.URL
import org.opentorah.entity.{EntitiesList, Entity}
import org.opentorah.util.Files
import org.opentorah.xml.{From, Parser}

final class Entities(
  inheritedSelectors: Seq[Selector],
  url: URL,
  element: EntitiesElement
) extends WithSelectors(inheritedSelectors) {
  def selector: Selector.Nullary = selectorByName(element.selector).asNullary

  val by: Entities.EntitiesBy = new Entities.EntitiesBy(selectors, url, element.by)

  val lists: Seq[EntitiesList] = element.lists.map(_.take(by.stores.map(_.entity)))

  def findByRef(ref: String): Option[Entity] = by.stores.find(_.entity.id.get == ref).map(_.entity)
}

object Entities {

  final class EntitiesBy(
    inheritedSelectors: Seq[Selector],
    url: URL,
    element: ByElement
  ) extends By.FromElement(inheritedSelectors, url, element) {

    override val stores: Seq[EntityStore] = {
      val directoryUrl: URL = Files.subdirectory(url, element.directory.get)
      Parser.parseDo(Parser.collectAll(
        for (fileName <- filesWithExtensions(url, extension = "xml")) yield parseStore(
          Files.fileInDirectory(directoryUrl, fileName + ".xml"),
          fileName
        )
      ))
    }

    private def parseStore(url: URL, fileName: String): Parser[EntityStore] = {
      val result: Parser[Entity] = for {
        result <- Entity.parse(From.url(url))
        _ <- Parser.check(result.id.isEmpty || result.id.contains(fileName),
          s"Incorrect id: ${result.id.get} instead of $fileName")
      } yield result.copy(
        id = Some(fileName)
      )

      result.map(entity => new EntityStore(selectors, url, entity))
    }
  }
}
