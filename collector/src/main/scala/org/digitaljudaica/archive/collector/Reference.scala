package org.digitaljudaica.archive.collector

import scala.xml.Elem
import Xml.Ops

final class Reference(
  val source: DocumentLike,
  val entity: Entity,
  xml: Elem
) {
  val name: String = xml.text
  val id: Option[String] = xml.attributeOption("xml:id")
  val role: Option[String] = xml.attributeOption("role")
  val ref: Option[String] = xml.attributeOption("ref")

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
