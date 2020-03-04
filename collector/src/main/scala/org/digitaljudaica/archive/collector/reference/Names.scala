package org.digitaljudaica.archive.collector.reference

import java.io.File
import org.digitaljudaica.archive.collector.{CollectionLike, Util}
import scala.xml.{Node, Text}

final class Names(
  override val reference: String,
  storeNameds: Seq[org.digitaljudaica.reference.Named],
  storeNamesLists: Seq[org.digitaljudaica.reference.NamesList],
  namedUrl: String => String,
  namedInTheListUrl: String => String
) extends CollectionLike {

  val nameds: Seq[Named] = for (teiNamed <- storeNameds) yield new Named(
    teiNamed,
    container = this,
    namedUrl,
    namedInTheListUrl
  )

  private val lists: Seq[NamesList] = for (storeNamesList <- storeNamesLists) yield new NamesList(
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

  def writeList(directory: File, fileName: String, namedInTheListUrl: String => String): Unit = {
    // List of all names
    val nonEmptyLists = lists.filterNot(_.isEmpty)

    val listOfLists: Seq[Node] =
      <p>{for (list <- nonEmptyLists) yield
        <l>{<ref target={namedInTheListUrl(list.id)} role="namesViewer">{list.head}</ref>}</l>
      }</p>

    Util.writeTei(
      directory = directory,
      fileName = fileName,
      head = Some(Text(reference)),
      content = listOfLists ++ nonEmptyLists.flatMap(_.toXml),
      target = "namesViewer"
    )
  }
}
