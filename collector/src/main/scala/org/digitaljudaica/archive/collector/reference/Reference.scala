package org.digitaljudaica.archive.collector.reference

import cats.implicits._
import org.digitaljudaica.archive.collector.Errors
import org.digitaljudaica.xml.{From, Parser, Xml}
import scala.xml.{Elem, Node}

final class Reference private(
  val source: ReferenceSource,
  val entity: Entity,
  val name: Seq[Node],
  val id: Option[String],
  val role: Option[String],
  val ref: Option[String]
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

  def apply(
    source: ReferenceSource,
    entity: Entity,
    xml: Elem
  ): Reference = From.xml(xml).mixed.parseDo(parser(source, entity))

  private def parser(
    source: ReferenceSource,
    entity: Entity,
  ): Parser[Reference] = for {
    id <- Xml.attribute.optional.id
    role <- Xml.attribute.optional("role")
    ref <- Xml.attribute.optional("ref")
    _ <- Xml.attribute.optional("type") // TODO we don't do anything with the type yet
    name <- Xml.allNodes
  } yield new Reference(
    source,
    entity,
    name,
    id,
    role,
    ref
  )
}
