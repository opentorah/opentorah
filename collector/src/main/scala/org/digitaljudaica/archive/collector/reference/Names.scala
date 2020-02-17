package org.digitaljudaica.archive.collector.reference

import java.io.File
import cats.implicits._
import org.digitaljudaica.archive.collector.{CollectionLike, Errors, Layout, Util}
import org.digitaljudaica.xml.{From, Parser, Xml}
import org.digitaljudaica.util.Files

import scala.xml.{Elem, Text}

final class Names private(
  directory: File,
  layout: Layout,
  override val reference: String,
  listDescriptors: Seq[NamesList.Descriptor]
) extends CollectionLike {

  val errors: Errors = new Errors
  val nameds: Seq[Named] = {
    for (fileName <- Files.filesWithExtensions(directory, extension = "xml").sorted) yield Named(
      directory,
      fileName,
      container = this,
      layout,
      errors
    )
  }
  errors.check()

  private val lists: Seq[NamesList] = listDescriptors.map(_.fillOut(nameds))

  private val references: Seq[Reference] = nameds.flatMap(_.references)

  def findByRef(ref: String): Option[Named] = nameds.find(_.id == ref)

  def processReferences(documentReferences: Seq[Reference]): Unit = {
    val references: Seq[Reference] = (this.references ++ documentReferences).filterNot(_.name == Text("?"))
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
      head = Some(Text(reference)),
      content,
      target = "namesViewer"
    )
  }
}

object Names {

  private def parser(
    directory: File,
    layout: Layout,
  ): Parser[Names] = for {
    reference <- Xml.element.characters.required("head", Xml.characters.required)
    listDescriptors <- Xml.element.elements.all(NamesList.parser)
  } yield new Names(
    directory,
    layout,
    reference,
    listDescriptors
  )

  def apply(directory: File, layout: Layout): Names =
    From.file(layout.docs, layout.namesListsFileName)
      .elements.parseDo(Xml.withName("names", parser(directory, layout)))
}
