package org.opentorah.reference

import java.io.File
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, ContentType, Element, From, Parsable, Parser, ToXml, XmlUtil}
import scala.xml.{Elem, Node}

final case class Named private(
  id: Option[String],
  entity: Entity,
  role: Option[String],
  names: Seq[Name],
  content: Seq[Node]
)

object Named extends Parsable[Named] with ToXml[Named] {

  override def toString: String = "Named"

  override val name2parser: Map[String, Parsable.ContentTypeAndParser[Named]] =
    (for (entity <- Entity.values)
    yield entity.element -> new Parsable.ContentTypeAndParser[Named](ContentType.Elements, contentParser(entity))).toMap

  private def contentParser(entity: Entity): Parser[Named] = for {
    id <- Attribute("id").optional
    role <- Attribute("role").optional
    names <- Name.parsable(entity).all
    _ <- Parser.check(names.nonEmpty, s"No names in $id")
    content <- Element.allNodes
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
    } yield checkId(fileName, Named.parse(From.file(directory, fileName)))
  ))

  private def checkId(fileName: String, parser: Parser[Named]): Parser[Named] = for {
    result <- parser
    _ <- Parser.check(result.id.isEmpty || result.id.contains(fileName),
      s"Incorrect id: ${result.id.get} instead of $fileName")
  } yield result.copy(id = Some(fileName))

  override def toXml(value: Named): Elem = {
    <elem id={value.id.orNull} role={value.role.orNull}>
      {value.names.map(_.toXml)}
      {value.content}
    </elem>
      .copy(label = value.entity.element)
  }
}
