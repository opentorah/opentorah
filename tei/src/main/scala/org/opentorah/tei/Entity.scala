package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parsable, Parser, ToXml, Xml}

final case class Entity private(
  id: Option[String],
  entityType: EntityType,
  role: Option[String],
  names: Seq[EntityName],
  content: Seq[Xml.Node]
) {
  def name: String = names.head.name

  def entityName: EntityName = names.head.copy(ref = Some(id.get))
}

object Entity {

  private val roleAttribute: Attribute[String] = Attribute("role")

  override def toString: String = "Entity"

  final val parsable: Parsable[Entity] with ToXml[Entity] = Parsable.union[Entity](
    _.entityType.element,
    EntityType.values.map(mkParsable)
  )

  private def mkParsable(entityType: EntityType): Element.WithToXml[Entity] =
    new Element.WithToXml[Entity](entityType.element) {
      override def contentType: ContentType = ContentType.Elements

      override def parser: Parser[Entity] = for {
        id <- Xml.idAttribute.optional
        role <- roleAttribute.optional
        names <- EntityName.getParsable(entityType).all
        _ <- Parser.check(names.nonEmpty, s"No names in $id")
        content <- Element.allNodes
      } yield new Entity(
        id,
        entityType,
        role,
        names,
        content,
      )

      override protected val antiparser: Antiparser[Entity] = Tei.concat(
        Xml.idAttribute.toXmlOption.compose(_.id),
        roleAttribute.toXmlOption.compose(_.role),
        EntityName.parsable.toXmlSeq.compose(_.names),
        Antiparser.xml.compose[Entity](_.content)
      )
    }
}
