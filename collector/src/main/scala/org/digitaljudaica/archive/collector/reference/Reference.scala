package org.digitaljudaica.archive.collector.reference

import org.digitaljudaica.reference.Entity
import scala.xml.{Elem, Node}

final case class Reference(
  source: ReferenceSource,
  reference: org.digitaljudaica.reference.Reference
) {
  override def toString: String = source.toString

  def entity: Entity = reference.entity
  def name: Seq[Node] = reference.name
  def id: Option[String] = reference.id
  def role: Option[String] = reference.role
  def ref: Option[String] = reference.ref

  def check(names: Names): Option[String] = {
    ref.fold[Option[String]](Some(s"Missing 'ref' attribute: Name>$name< ($source)")) { ref =>
      if (ref.contains(" ")) Some(s"""Value of the ref attribute contains spaces: ref="$ref" """) else {
        names.findByRef(ref).fold[Option[String]](Some(s"""Unresolvable reference: Name ref="$ref">${name.text}< """)) { named =>
          if (named.entity != entity) Some(s"$entity reference to ${named.entity} ${named.name}: $name [$ref]")
          else None
        }
      }
    }
  }

  def toXml: Elem =
    <name ref={ref.orNull} xml:id={id.orNull} role={role.orNull}>{name}</name>
      .copy(label = entity.nameElement)
}
