package org.digitaljudaica.archive.collector

import scala.xml.Elem
import Xml.Ops

final case class Reference(
  source: HasReferences,
  name: String,
  id: Option[String],
  role: Option[String],
  ref: Option[String],
  entity: Entity
) {
  override def toString: String = source.toString

  def toXml: Elem =
    <name ref={ref.orNull} xml:id={id.orNull} role={role.orNull}>{name}</name>
      .copy(label = entity.nameElement)
}

object Reference {
  def parseReferences(document: HasReferences, xml: Elem, errors: Errors): Seq[Reference] =
    Entity.values.flatMap { entity =>
      for (elem <- xml.descendants(entity.nameElement)) yield {
        val ref: Option[String] = elem.attributeOption("ref").flatMap { ref: String =>
          if (ref.contains(" ")) {
            errors.error(s"""Value of the ref attribute contains spaces: ref="$ref" """)
            None
          } else Some(ref)
        }

        Reference(
          document,
          name = elem.text,
          id = elem.idOption,
          role = elem.attributeOption("role"),
          ref,
          entity
        )
      }
    }
}
