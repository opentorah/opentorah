package org.digitaljudaica.archive.collector.reference

import org.digitaljudaica.archive.collector.Errors
import org.digitaljudaica.tei.Entity
import scala.xml.{Elem, Node}

final case class Reference(
  source: ReferenceSource,
  entity: Entity,
  name: Seq[Node],
  id: Option[String],
  role: Option[String],
  ref: Option[String]
) {

  override def toString: String = source.toString

  def check(names: Names, errors: Errors): Unit = {
    ref.fold(errors.error(s"Missing 'ref' attribute: Name>$name< ($source)")) { ref =>
      if (ref.contains(" ")) errors.error(s"""Value of the ref attribute contains spaces: ref="$ref" """) else {
        names.findByRef(ref).fold(errors.error(s"""Unresolvable reference: Name ref="$ref">${name.text}< """)) { named =>
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
  def apply(source: ReferenceSource, teiReference: org.digitaljudaica.tei.Reference): Reference = new Reference(
    source,
    teiReference.entity,
    teiReference.name,
    teiReference.id,
    teiReference.role,
    teiReference.ref
  )
}
