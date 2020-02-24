package org.digitaljudaica.reference

import org.digitaljudaica.tei.Tei
import org.digitaljudaica.xml.{ContentType, XmlUtil, Parser, Xml}
import scala.xml.Elem

final case class Named private(
  entity: Entity,
  id: String,
  role: Option[String],
  names: Seq[Name], // TODO split ito main and alternatives
  content: Seq[Elem] // TODO switch to Seq[Node]
)

object Named {
  def parser(id: String): Parser[Named] =
    unwrapTei(contentParser(id))

  private def contentParser(id: String): Parser[Named] = for {
    name <- Xml.name
    entityOption = Entity.forElement(name)
    _ <- Parser.check(entityOption.isDefined, s"No such entity type: $name")
    entity = entityOption.get
    idOption <- Xml.attribute.optional.id
    _ <- Parser.check(idOption.isEmpty || idOption.contains(id), s"Wrong id ${idOption.get} in $id")
    role <- Xml.attribute.optional("role")
    names <- Xml.all(entity.nameElement, ContentType.Text, Name.parser(entity))
    _ <- Parser.check(names.nonEmpty, s"No names in $id")
    content <- Xml.all // TODO allNodes
  } yield new Named(
    entity,
    id,
    role,
    names,
    content = content.map(XmlUtil.removeNamespace),
  )

  private def unwrapTei(parser: Parser[Named]): Parser[Named] = for {
    name <- Xml.name
    result <- if (name != Tei.elementName) parser else for {
      tei <- Tei.contentParser
      result <- Xml.nested("TEI Wrapper", tei.body.xml, ContentType.Elements, Xml.required(parser))
    } yield result
  } yield result
}
