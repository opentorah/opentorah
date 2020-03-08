package org.opentorah.archive.collector.reference

import org.opentorah.archive.collector.CollectionLike
import scala.xml.Text

final class Names(
  override val reference: String,
  storeNameds: Seq[org.opentorah.reference.Named],
  storeNamesLists: Seq[org.opentorah.reference.NamesList],
  namedUrl: String => String,
  namedInTheListUrl: String => String
) extends CollectionLike {

  val nameds: Seq[Named] = for (teiNamed <- storeNameds) yield new Named(
    teiNamed,
    container = this,
    namedUrl,
    namedInTheListUrl
  )

  val lists: Seq[NamesList] = for (storeNamesList <- storeNamesLists) yield new NamesList(
    storeNamesList = storeNamesList,
    nameds = nameds.filter(named => storeNamesList.includes(named.storeNamed))
  )

  def findByRef(ref: String): Option[Named] = nameds.find(_.id == ref)

  private var references: Seq[Reference] = _
  def getReferences: Seq[Reference] = references

  def addDocumentReferences(documentReferences: Seq[Reference]): Unit = {
    references = (nameds.flatMap(_.references) ++ documentReferences).filterNot(_.name == Text("?"))
  }

  def checkReferences(): Unit = {
    val errors: Seq[String] = references.flatMap(_.check(this))
    if (errors.nonEmpty) throw new IllegalArgumentException(errors.mkString("\n"))
  }
}
