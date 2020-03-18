package org.opentorah.reference

import java.net.URL
import org.opentorah.metadata.{Language, Names}
import org.opentorah.store.{By, FilesList, Nameds, Path, Selector, Store}
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, ContentType, Element, From, Parsable, Parser, ToXml, XmlUtil}
import scala.xml.{Elem, Node}

final case class Named private(
  url: Option[URL],
  id: Option[String], // TODO make id non-optional and provide toXml() flavour that drops id.
  entity: Entity,
  role: Option[String],
  namedNames: Seq[Name],
  content: Seq[Node]
) extends Store {

  def name: String = namedNames.head.name

  override def names: Names = {
    val russianName = name
    val englishName = id.get
    new Names(
      Seq(org.opentorah.metadata.Name(russianName, Language.Russian)) ++
      (if (englishName == russianName) Seq.empty else Seq(org.opentorah.metadata.Name(englishName, Language.English)))
    )
  }

  override def selectors: Seq[Selector] = Seq.empty

  override def nameds: Option[Nameds] = None

  override def by: Option[By] = None

  override def references(at: Path): Seq[Reference] =
    content.flatMap(element => Reference.parsable.descendants(element)).map(_.at(at))
}

object Named extends Parsable[Named] with ToXml[Named] {

  override def toString: String = "Named"

  override val name2parser: Map[String, Parsable.ContentTypeAndParser[Named]] = Entity.values.map { entity =>
    entity.element -> new Parsable.ContentTypeAndParser[Named](ContentType.Elements, parser(entity))
  }.toMap

  private def parser(entity: Entity): Parser[Named] = for {
    id <- Attribute("id").optional
    role <- Attribute("role").optional
    names <- Name.parsable(entity).all
    _ <- Parser.check(names.nonEmpty, s"No names in $id")
    content <- Element.allNodes
  } yield new Named(
    url = None,
    id,
    entity,
    role,
    names,
    content = content.map(XmlUtil.removeNamespace),
  )

  def parseAll(baseUrl: URL, directory: String, list: String): Parser[Seq[Named]] = {
    val directoryUrl = Files.subdirectory(baseUrl, directory)
    val fileNames: Seq[String] = FilesList.filesWithExtensions(
      directoryUrl,
      Files.fileInDirectory(baseUrl, list),
      extension = "xml"
    )

    Parser.collectAll(
      for (fileName <- fileNames) yield {
        val url: URL = Files.fileInDirectory(directoryUrl, fileName + ".xml")
        for {
          result <- Named.parse(From.url(url))
          _ <- Parser.check(result.id.isEmpty || result.id.contains(fileName),
            s"Incorrect id: ${result.id.get} instead of $fileName")
        } yield result.copy(
          url = Some(url),
          id = Some(fileName)
        )
      }
    )
  }

  override def toXml(value: Named): Elem = {
    <elem id={value.id.orNull} role={value.role.orNull}>
      {value.namedNames.map(Name.toXml)}
      {value.content}
    </elem>
      .copy(label = value.entity.element)
  }
}
