package org.digitaljudaica.archive.collector.reference

import java.io.File
import org.digitaljudaica.archive.collector.{CollectionLike, Errors, Layout, Util}
import org.digitaljudaica.xml.{ContentType, Error, From, Parser, Xml}
import org.digitaljudaica.util.Files
import zio.{IO, ZIO}

import scala.xml.{Elem, Text}

final class Names private(
  directory: File,
  layout: Layout,
  override val reference: String,
  teiNameds: Seq[org.digitaljudaica.reference.Named],
  listDescriptors: Seq[NamesList.Descriptor]
) extends CollectionLike {

  val nameds: Seq[Named] = for (teiNamed <- teiNameds) yield new Named(teiNamed, container = this, layout)

  private val lists: Seq[NamesList] = listDescriptors.map(_.fillOut(nameds))

  private val references: Seq[Reference] = nameds.flatMap(_.references)

  def findByRef(ref: String): Option[Named] = nameds.find(_.id == ref)

  def processReferences(documentReferences: Seq[Reference]): Unit = {
    val errors: Errors = new Errors
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

  def apply(directory: File, layout: Layout): Names =
    Parser.parseDo(From.file(layout.docs, layout.namesListsFileName).parse(
      Xml.withName("names", parser(directory, layout))))

  private def parser(
    directory: File,
    layout: Layout,
  ): Parser[Names] = for {
    reference <- Xml.required("head", ContentType.Text, Xml.text.required)
    listDescriptors <- Xml.all(NamesList.parser)
    teiNamedResults <-
      ZIO.collectAll(Files.filesWithExtensions(directory, extension = "xml").sorted.map(fileName =>
        From.file(directory, fileName).parse(org.digitaljudaica.reference.Named.parser(fileName)).either))
    teiNameds <- {
      val errors: Seq[Error] = teiNamedResults.flatMap(_.left.toOption)
      val results: Seq[org.digitaljudaica.reference.Named] = teiNamedResults.flatMap(_.right.toOption)
      if (errors.nonEmpty) IO.fail(errors.mkString("--", "\n--", "")) else IO.succeed(results)
    }
  } yield new Names(
    directory,
    layout,
    reference,
    teiNameds,
    listDescriptors
  )
}
