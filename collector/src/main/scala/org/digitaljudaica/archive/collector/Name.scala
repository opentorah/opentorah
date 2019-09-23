package org.digitaljudaica.archive.collector

import scala.xml.Elem
import Xml.Ops

final case class Name(
  document: DocumentLike,
  name: String,
  id: Option[String],
  role: Option[String],
  refAttribute: Option[String],
  ref: Option[String],
  entity: Entity
) {
  override def toString: String = document.toString

  def toXml: Elem = {
    <name ref={refAttribute.orNull} xml:id={id.orNull} role={role.orNull}>{name}</name>
      .copy(label = entity.nameElement)
  }
}

object Name {
  def parse(entity: Entity, document: DocumentLike, xml: Elem, errors: Errors): Name = {
    xml.check(entity.nameElement)

    val refAttribute: Option[String] = xml.attributeOption("ref")

    val ref: Option[String] = refAttribute.flatMap { refAttribute: String =>
      if (!refAttribute.startsWith("#")) {
        errors.error(s"""Value of the 'ref' attribute does not start with '#': ref="$refAttribute" """)
        None
      } else if (refAttribute.contains(" ")) {
        errors.error(s"""Value of the ref attribute contains spaces: ref="$refAttribute" """)
        None
      } else Some(refAttribute.substring(1))
    }

    Name(
      document,
      name = xml.text,
      id = xml.attributeOption("xml:id"),
      role = xml.attributeOption("role"),
      refAttribute,
      ref,
      entity
    )
  }

  def parseNames(entity: Entity, document: DocumentLike, xml: Seq[Elem], errors: Errors): Seq[Name] =
    xml.map(elem => Name.parse(entity, document, elem, errors))

  def parseAllNames(document: DocumentLike, xml: Elem, errors: Errors): Seq[Name] =
    Entity.values.flatMap(entity => parseNames(entity, document, xml.descendants(entity.nameElement), errors))
}
