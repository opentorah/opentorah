package org.digitaljudaica.archive.collector

import java.io.File

import scala.xml.{Elem, Text}
import Xml.Ops

final class Names(directory: File, layout: Layout) extends CollectionLike {
  private val xml: Elem = Xml.load(layout.docs, layout.namesListsFileName).check("names")
  private val elements: Seq[Elem] = xml.elements

  private val head: String = elements.head.check("head").text
  override def reference: String = head

  private val errors: Errors = new Errors
  private val nameds: Seq[Named] = {
    for (fileName <- Util.filesWithExtensions(directory, extension = ".xml").sorted) yield new Named(
      rawXml = Xml.load(directory, fileName),
      fileName,
      container = this,
      layout,
      errors
    )
  }
  errors.check()

  private val lists: Seq[NamesList] = elements.tail.map(element => new NamesList(element, nameds))

  private val references: Seq[Reference] = nameds.flatMap(_.references)

  def findByRef(ref: String): Option[Named] = nameds.find(_.id == ref)

  def processReferences(documentReferences: Seq[Reference]): Unit = {
    val references: Seq[Reference] = (this.references ++ documentReferences).filterNot(_.name == "?")
    for (reference <- references) reference.check(this, errors)
    errors.check()

    // Individual names
    for (named <- nameds) Util.writeTei(
      directory,
      fileName = named.id,
      head = None,
      content = named.toXml(references),
      target = "namesViewer"
    )

    // List of all names
    val nonEmptyLists = lists.filterNot(_.isEmpty)
    val content: Seq[Elem] =
      <p>{for (list <- nonEmptyLists)
        yield <l><ref target={layout.namedInTheListUrl(list.id)} role="namesViewer">{list.head}</ref></l>}</p> +:
        (for (list <- nonEmptyLists) yield list.toXml)

    Util.writeTei(
      directory = layout.namesFileDirectory,
      fileName = layout.namesFileName,
      head = Some(Text(head)),
      content,
      target = "namesViewer"
    )
  }
}
