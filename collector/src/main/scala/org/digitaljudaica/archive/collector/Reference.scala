package org.digitaljudaica.archive.collector

import scala.xml.Elem
import Xml.Ops

final case class Reference(
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

object Reference {
  def parseReferences(document: DocumentLike, xml: Elem, errors: Errors): Seq[Reference] =
    Entity.values.flatMap { entity =>
      for (elem <- xml.descendants(entity.nameElement))
      yield parse(entity, document, elem, errors)
    }

  private def parse(entity: Entity, document: DocumentLike, xml: Elem, errors: Errors): Reference = {
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

    Reference(
      document,
      name = xml.text,
      id = xml.attributeOption("xml:id"),
      role = xml.attributeOption("role"),
      refAttribute,
      ref,
      entity
    )
  }
}
