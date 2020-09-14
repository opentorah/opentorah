package org.opentorah.tei

import org.opentorah.util.Files
import org.opentorah.xml.{Antiparser, Attribute, ContentType, Element, Parsable, Parser, ToXml, Xml}

final class EntityName private(
  val entityType: EntityType,
  val id: Option[String],
  val name: String
)

object EntityName extends ToXml[EntityName] {

  def parsable(entityType: EntityType): Parsable[EntityName] = new Element[EntityName](entityType.nameElement) {
    override protected def contentType: ContentType = ContentType.Characters

    override protected def parser: Parser[EntityName] = for {
      id <- Xml.idAttribute.optional
      name <- org.opentorah.xml.Text().required
    } yield new EntityName(entityType, id, name)
  }

  override protected def elementName(value: EntityName): String = value.entityType.nameElement

  override protected val antiparser: Antiparser[EntityName] = Antiparser.concat(
    Xml.idAttribute.toXmlOption.compose[EntityName](_.id),
    Antiparser.xml.compose[EntityName](value => Seq(Xml.mkText(value.name)))
  )

  private val targetAttribute: Attribute[String] = Attribute("target")
  private val roleAttribute: Attribute[String] = Attribute("role")
  private val refAttribute: Attribute[String] = Attribute("ref")

  def transformer(resolver: TeiResolver): Xml.Transformer = elem =>
    if (!EntityType.isName(elem.label)) elem else {
      refAttribute.get(elem).fold(elem) { ref =>
        resolver.findByRef(ref).fold(elem) { resolved =>
          Attribute.setAll(elem, Seq(
            roleAttribute.withValue(resolved.role.orNull),
            targetAttribute.withValue(Files.mkUrl(resolved.url))
          ))
        }
      }
    }
}
