package org.digitaljudaica.archive.collector

import scala.xml.{Elem, Text}
import Xml.Ops

final class Names(layout: Layout, errors: Errors) extends CollectionLike {
  val xml: Elem = Xml.load(layout.docs, layout.namesListsFileName).check("names")
  val elements: Seq[Elem] = xml.elements

  val head: String = elements.head.check("head").text
  override def reference: String = head

  val nameds: Seq[Named] = Named.parse(layout, namesContainer = this, layout.namesDirectory, errors)
  val lists: Seq[NamesList] = elements.tail.map(element => NamesList(element, nameds))

  errors.check()

  val references: Seq[Reference] = nameds.flatMap(_.references)

  def findByRef(ref: String): Option[Named] = nameds.find(_.id == ref)

  def processReferences(documentReferences: Seq[Reference]): Unit = {
    val references: Seq[Reference] = (this.references ++ documentReferences).filterNot(_.name == "?")

    for (reference <- references) reference.check(this, errors)
    errors.check()

    // Individual names
    for (named <- nameds) Util.writeTei(
      directory = layout.namesDirectory,
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
        (for (list <- nonEmptyLists) yield
          <list xml:id={list.id} role={list.role.orNull}>
            <head>{list.head}</head>
            {for (named <- list.nameds) yield named.toListXml}
          </list>
            .copy(label = list.entity.listElement))

    Util.writeTei(
      directory = layout.namesFileDirectory,
      fileName = layout.namesFileName,
      head = Some(Text(head)),
      content,
      target = "namesViewer"
    )
  }
}
