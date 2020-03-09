package org.opentorah.reference

import java.io.File
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, ContentType, Element, From, Parser, XmlUtil}
import scala.xml.Node

// TODO extend Repeatable
final case class Named private(
  id: String,
  entity: Entity,
  role: Option[String],
  names: Seq[Name],
  content: Seq[Node]
)

object Named {

  def contentParser(id: String): Parser[Named] = for {
    name <- Element.name
    entityOption = Entity.forElement(name)
    _ <- Parser.check(entityOption.isDefined, s"No such entity type: $name")
    entity = entityOption.get
    role <- Attribute("role").optional
    names <- Element(entity.nameElement, ContentType.Text, Name.parser(entity)).all
    _ <- Parser.check(names.nonEmpty, s"No names in $id")
    content <- Parser.allNodes
  } yield new Named(
    id,
    entity,
    role,
    names,
    content = content.map(XmlUtil.removeNamespace),
  )

  def readAll(directory: File): Seq[Named] = Parser.parseDo(Parser.collectAll(
    for {
      fileName <- Files.filesWithExtensions(directory, extension = "xml").sorted
    } yield From.file(directory, fileName)
      .parse(contentParser(fileName))
  ))
}
