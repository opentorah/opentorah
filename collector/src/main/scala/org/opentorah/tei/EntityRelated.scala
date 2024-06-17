package org.opentorah.tei

import org.opentorah.xml.{ContentType, ElementTo, ElementsTo, Parsable}

abstract class EntityRelated[E](
  elementName: EntityType => String,
  entityType: E => EntityType
) extends ElementsTo.Union[E]:
  protected def contentType: ContentType

  protected def parsable(entityType: EntityType): Parsable[E]

  sealed class ForEntityType(entityType: EntityType) extends ElementTo[E](elementName(entityType)):
    override def contentType: ContentType = EntityRelated.this.contentType
    override def contentParsable: Parsable[E] = EntityRelated.this.parsable(entityType)

  object Person       extends ForEntityType(EntityType.Person      )
  object Place        extends ForEntityType(EntityType.Place       )
  object Organization extends ForEntityType(EntityType.Organization)

  final override protected val elements: Seq[ForEntityType] = Seq(Person, Place, Organization)

  final override protected def elementByValue(value: E): ElementTo[E] = forEntityType(entityType(value))

  final def forEntityType(entityType: EntityType): ElementTo[E] = entityType match
    case EntityType.Person       => Person
    case EntityType.Place        => Place
    case EntityType.Organization => Organization
