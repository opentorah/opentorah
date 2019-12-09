package org.digitaljudaica.archive.collector

import scala.xml.Elem
import Xml.Ops

final case class Reference(
  source: DocumentLike,
  name: String,
  id: Option[String],
  role: Option[String],
  ref: Option[String],
  entity: Entity
) {
  override def toString: String = source.toString

  def check(names: Names, errors: Errors): Unit = {
    ref.fold(errors.error(s"Missing 'ref' attribute: Name>$name< ($source)")) { ref =>
      if (ref.contains(" ")) errors.error(s"""Value of the ref attribute contains spaces: ref="$ref" """) else {
        names.findByRef(ref).fold(errors.error(s"""Unresolvable reference: Name ref="$ref">$name< """)) { named =>
          if (named.entity != entity) errors.error(s"$entity reference to ${named.entity} ${named.name}: $name [$ref]")
        }
      }
    }
  }

  def toXml: Elem =
    <name ref={ref.orNull} xml:id={id.orNull} role={role.orNull}>{name}</name>
      .copy(label = entity.nameElement)
}

object Reference {

  def apply(source: DocumentLike, xml: Elem, entity: Entity): Reference = Reference(
    source,
    name = xml.text,
    id = xml.attributeOption("xml:id"),
    role = xml.attributeOption("role"),
    ref = xml.attributeOption("ref"),
    entity
  )
}
