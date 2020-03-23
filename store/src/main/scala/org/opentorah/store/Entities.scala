package org.opentorah.store

import java.net.URL
import org.opentorah.entity.{EntitiesList, Entity, EntityReference}
import org.opentorah.util.Files
import org.opentorah.xml.{From, Parser}

// TODO turn into Store (with two Bys) (and then filesWithExtensions()'s url parameter will always be store.url :))
final class Entities(
  store: Store,
  url: URL,
  element: EntitiesElement
) {
  def selector: Selector.Nullary = store.selectorByName(element.selector).asNullary

  val by: Entities.EntitiesBy = new Entities.EntitiesBy(store, url, element.by)

  val lists: Seq[EntitiesList] = element.lists.map(_.take(by.stores.map(_.entity)))

  def findByRef(ref: String): Option[Entity] = by.stores.find(_.entity.id.get == ref).map(_.entity)

  def references(at: Path): Seq[EntityReference] = by.references(at :+ selector.bind)
}

object Entities {

  final class EntitiesBy(store: Store, url: URL, element: ByElement) extends By.FromElement(store, element) {

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

      result.map(entity => new EntityStore(parent = None, url, entity))
    }
  }
}
