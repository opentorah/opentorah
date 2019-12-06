package org.digitaljudaica.archive.collector

import java.io.File

import scala.xml.Elem
import Xml.Ops

final class Names(layout: Layout, errors: Errors) {
  val xml: Elem = Xml.load(layout.namesDirectory, layout.namesFileName).check("names")
  val elements: Seq[Elem] = xml.elements
  val head: String = elements.head.check("head").text

  val lists: Seq[Nameds] = elements.tail.map(element => Nameds.parse(layout, this, element, layout.namesDirectory, errors))

  for (orphanDirectoryName <-
         layout.namesDirectory.listFiles.filter(_.isDirectory).map(_.getName).toSet -- lists.map(_.name).toSet)
    errors.error(s"Orphan names directory: $orphanDirectoryName")

  for ((id, nameds) <- lists.groupBy(_.name).filter(_._2.length != 1)) {
    errors.error(s"Duplicate list ids: $id - $nameds")
  }

  private val nameds: Seq[Named] = lists.flatMap(_.nameds)

  for ((id, nameds) <- nameds.groupBy(_.id).filter(_._2.length != 1)) {
    errors.error(s"Duplicate named ids: $id - $nameds")
  }

  errors.check()

  val references: Seq[Reference] = nameds.flatMap(_.references)

  def isResolvable(name: Reference): Boolean = nameds.exists(_.id.contains(name.ref.get))

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

    val directory: File = layout.namesFileDirectory
    val fileName: String = layout.namesFileName

    val content: Seq[Elem] =
      <p>{for (list <- lists) yield <l><ref target={layout.namedUrl(list.name)} role="namesViewer">{list.head}</ref></l>}</p> +:
         (for (list <- lists) yield list.addMentions(references).toXml)

    Tei.tei(head, content).write(directory, fileName)

    // Wrapper
    Util.writeTeiYaml(Util.htmlFile(directory, fileName),
      style = "names",
      tei = s"$fileName.xml",
      title = head,
      target = "namesViewer"
    )
  }
}
