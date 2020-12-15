package org.opentorah.tei

import org.opentorah.xml.{Antiparser, ContentType, Element, Parser, ToXml, Xml}

abstract class EntityRelated[E](
  elementName: EntityType => String,
  entityType: E => EntityType
) extends Element.Union[E] with ToXml[E] {
  protected def contentType: ContentType

  protected def parser(entityType: EntityType): Parser[E]

  protected def antiparser(entityType: EntityType): Antiparser[E]

  sealed class ForEntityType(entityType: EntityType) extends Element[E](elementName(entityType)) {
    override def contentType: ContentType    = EntityRelated.this.contentType
    override def parser     : Parser     [E] = EntityRelated.this.parser     (entityType)
    override def antiparser : Antiparser [E] = EntityRelated.this.antiparser (entityType)
  }

  final object Person       extends ForEntityType(EntityType.Person      )
  final object Place        extends ForEntityType(EntityType.Place       )
  final object Organization extends ForEntityType(EntityType.Organization)

  final override protected val elements: Seq[ForEntityType] = Seq(Person, Place, Organization)

  // TODO extend Union from ToXml:
  final override def toXmlElement(value: E): Xml.Element =
    forEntityType(entityType(value)).toXmlElement(value)

  final def forEntityType(entityType: EntityType): Element[E] = entityType match {
    case EntityType.Person       => Person
    case EntityType.Place        => Place
    case EntityType.Organization => Organization
  }
}
