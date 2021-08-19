package org.opentorah.tei

import org.opentorah.util.Effects
import org.opentorah.xml.{Attribute, ContentType, Element, Parsable, Parser, ScalaXml, Unparser, Xml}

final case class Entity(/* TODO private? */
  id: Option[String],
  entityType: EntityType,
  role: Option[String],
  names: Seq[EntityName],
  content: ScalaXml.Nodes // TODO rename body?
) {
  def name: String = names.head.name

  def entityName: EntityName = names.head.copy(ref = Some(id.get))
}

object Entity extends EntityRelated[Entity](
  elementName = _.element,
  entityType = _.entityType
) {
  // TODO: with this, in Scala 3 I get 'Double definition' error:
  // override def toString: String = "Entity"

  override protected def contentType: ContentType = ContentType.Elements

  private val idAttribute: Attribute.Optional[String] = Xml.idAttribute.optional
  private val roleAttribute: Attribute.Optional[String] = Attribute("role").optional

  override protected def parsable(entityType: EntityType): Parsable[Entity] = new Parsable[Entity] {
    private val namesParsable: Parsable[Seq[EntityName]] = EntityName.forEntityType(entityType).seq

    override protected def parser: Parser[Entity] = for {
      id <- idAttribute()
      role <- roleAttribute()
      names <- namesParsable()
      _ <- Effects.check(names.nonEmpty, s"No names in $id")
      content <- Element.nodes()
    } yield new Entity(
      id,
      entityType,
      role,
      names,
      content,
    )

    override def unparser: Unparser[Entity] = Tei.concat(
      idAttribute(_.id),
      roleAttribute(_.role),
      namesParsable(_.names),
      Element.nodes(_.content)
    )
  }
}
