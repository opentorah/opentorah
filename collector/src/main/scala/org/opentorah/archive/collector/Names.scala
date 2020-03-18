package org.opentorah.archive.collector

import org.opentorah.metadata.{Language, Name}
import org.opentorah.reference.{Named, NamesList, Reference}
import org.opentorah.store.{Binding, Path}
import scala.xml.Text

final class Names(
  val path: Path,
  storeNameds: Seq[Named], // TODO eliminate
  storeNamesLists: Seq[NamesList] // TODO eliminate
) {

  val nameds: Seq[Named] = for (teiNamed <- storeNameds) yield {
    val russianName = teiNamed.names.head.name
    val englishName = teiNamed.id.get

    val namedPath = path :+ Binding.Named(Selectors.Name, new org.opentorah.metadata.Names(
      Seq(new Name(russianName, Language.Russian.toSpec)) ++
      (if (englishName == russianName) Seq.empty else Seq(new Name(englishName, Language.English.toSpec)))))

    teiNamed.at(namedPath)
  }

  val lists: Seq[NamesList] = storeNamesLists.map(_.take(nameds))

  def findByRef(ref: String): Option[Named] = nameds.find(_.id.get == ref)

  private var references: Seq[Reference] = _
  def getReferences: Seq[Reference] = references

  def addDocumentReferences(documentReferences: Seq[Reference]): Unit = {
    references = (nameds.flatMap(_.references) ++ documentReferences).filterNot(_.name == Text("?"))
  }
}
