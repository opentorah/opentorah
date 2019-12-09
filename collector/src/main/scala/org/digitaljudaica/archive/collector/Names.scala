package org.digitaljudaica.archive.collector

import scala.xml.{Elem, Text}
import Xml.Ops

final class Names(layout: Layout, errors: Errors) {
  val xml: Elem = Xml.load(layout.docs, layout.namesListsFileName).check("names")
  val elements: Seq[Elem] = xml.elements
  val head: String = elements.head.check("head").text
  val nameds: Seq[Named] = Named.parse(layout, namesContainer = this, layout.namesDirectory, errors)
  val lists: Seq[NamesList] = elements.tail.map(element => NamesList(element, nameds))

  for ((id, nameds) <- lists.groupBy(_.id).filter(_._2.length != 1)) {
    errors.error(s"Duplicate list ids: $id - $nameds")
  }

  for ((id, nameds) <- nameds.groupBy(_.id).filter(_._2.length != 1)) {
    errors.error(s"Duplicate named ids: $id - $nameds")
  }

  errors.check()

  val references: Seq[Reference] = nameds.flatMap(_.references)

  def isResolvable(name: Reference): Boolean = {
    val result = nameds.find(_.id == name.ref.get)
    result.isDefined && {
      val entity = result.get.entity
      if (result.get.entity != name.entity) errors.error(s"${name.entity} reference to $entity ${result.get.name}: $name [${name.ref.get}]")
      true
    }
  }

  def processReferences(documentReferences: Seq[Reference]): Unit = {
    val references: Seq[Reference] = (this.references ++ documentReferences).filterNot(_.name == "?")

    for (reference <- references.filter(_.ref.isEmpty)) {
      errors.error(s"Missing 'ref' attribute: Name>${reference.name}< (${reference.source})")
    }

    errors.check()

    for (reference <- references.filterNot(reference => isResolvable(reference))) {
      errors.error(s"""Unresolvable reference: Name ref="${reference.ref.orNull}">${reference.name}< """)
    }

    errors.check()

    val nonEmptyLists = lists.filterNot(_.isEmpty)
    val content: Seq[Elem] =
      <p>{
        for (list <- nonEmptyLists)
          yield <l><ref target={layout.namedUrl(list.id)} role="namesViewer">{list.head}</ref></l>}
      </p> +:
         (for (list <- nonEmptyLists) yield list.addMentions(references).toXml)

    Util.writeTei(
      directory = layout.namesFileDirectory,
      fileName = layout.namesFileName,
      head = Text(head),
      content,
      target = "namesViewer"
    )
  }
}
