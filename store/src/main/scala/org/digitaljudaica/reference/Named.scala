package org.digitaljudaica.reference

import java.io.File
import org.digitaljudaica.util.Files
import org.digitaljudaica.xml.{ContentType, From, Parser, Xml, XmlUtil}
import scala.xml.Node

final case class Named private(
  entity: Entity,
  id: String,
  role: Option[String],
  names: Seq[Name],
  content: Seq[Node]
)

object Named {

  def contentParser(id: String): Parser[Named] = for {
    name <- Xml.name
    entityOption = Entity.forElement(name)
    _ <- Parser.check(entityOption.isDefined, s"No such entity type: $name")
    entity = entityOption.get
    idOption <- Xml.attribute.optional.id
    _ <- Parser.check(idOption.isEmpty || idOption.contains(id), s"Wrong id ${idOption.get} in $id")
    role <- Xml.attribute.optional("role")
    names <- Xml.all(entity.nameElement, ContentType.Text, Name.parser(entity))
    _ <- Parser.check(names.nonEmpty, s"No names in $id")
    content <- Xml.allNodes
  } yield new Named(
    entity,
    id,
    role,
    names,
    content = content.map(XmlUtil.removeNamespace),
  )

  def readAll(directory: File): Seq[org.digitaljudaica.reference.Named] = Parser.parseDo(Parser.collectAll(
    for {
      fileName <- Files.filesWithExtensions(directory, extension = "xml").sorted
    } yield From.file(directory, fileName)
      .parse(org.digitaljudaica.reference.Named.contentParser(fileName))
  ))
}
