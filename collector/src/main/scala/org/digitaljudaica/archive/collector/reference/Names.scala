package org.digitaljudaica.archive.collector.reference

import java.io.File
import org.digitaljudaica.archive.collector.{CollectionLike, Util}
import org.digitaljudaica.xml.{ContentType, Error, From, Parser, Xml}
import org.digitaljudaica.util.Files
import zio.{IO, ZIO}
import scala.xml.{Node, Text}

final class Names private(
  override val reference: String,
  teiNameds: Seq[org.digitaljudaica.reference.Named],
  listDescriptors: Seq[NamesList.Descriptor],
  namedUrl: String => String,
  namedInTheListUrl: String => String
) extends CollectionLike {

  val nameds: Seq[Named] =
    for (teiNamed <- teiNameds) yield new Named(
      teiNamed,
      container = this,
      namedUrl,
      namedInTheListUrl
    )

  private val lists: Seq[NamesList] = listDescriptors.map(_.fillOut(nameds))

  def findByRef(ref: String): Option[Named] = nameds.find(_.id == ref)

  private var references: Seq[Reference] = _

  def addDocumentReferences(documentReferences: Seq[Reference]): Unit = {
    references = (nameds.flatMap(_.references) ++ documentReferences).filterNot(_.name == Text("?"))
  }

  def checkReferences(): Unit = {
    val errors: Seq[String] = references.flatMap(_.check(this))
    if (errors.nonEmpty) throw new IllegalArgumentException(errors.mkString("\n"))
  }

  def writeNames(directory: File): Unit = for (named <- nameds) Util.writeTei(
    directory,
    fileName = named.id,
    head = None,
    content = named.toXml(references),
    target = "namesViewer"
  )

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

object Names {

  def parser(
    directory: File,
    namedUrl: String => String,
    namedInTheListUrl: String => String
  ): Parser[Names] = for {
    reference <- Xml.required("head", ContentType.Text, Xml.text.required)
    listDescriptors <- Xml.all(NamesList.parser)
    teiNamedResults <- ZIO.collectAll(Files.filesWithExtensions(directory, extension = "xml").sorted.map(fileName =>
        From.file(directory, fileName)
          .parse(ContentType.Elements, org.digitaljudaica.reference.Named.contentParser(fileName)).either))
    errors: Seq[Error] = teiNamedResults.flatMap(_.left.toOption)
    results: Seq[org.digitaljudaica.reference.Named] = teiNamedResults.flatMap(_.right.toOption)
    teiNameds <- if (errors.nonEmpty) IO.fail(errors.mkString("--", "\n--", "")) else IO.succeed(results)
  } yield new Names(
    reference,
    teiNameds,
    listDescriptors,
    namedUrl,
    namedInTheListUrl
  )
}
