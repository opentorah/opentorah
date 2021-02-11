package org.opentorah.collector

import org.opentorah.tei.EntityReference
import org.opentorah.xml.Xml

final class References(references: Seq[EntityReference]) {

  def verify(site: Site): Seq[String] = {
    def checkReference(reference: EntityReference): Option[String] = {
      val name: Xml.Nodes = reference.name
      reference.ref.fold[Option[String]](None) { ref =>
        if (ref.contains(" ")) Some(s"""Value of the ref attribute contains spaces: ref="$ref" """) else {
          site.entities.findByName(ref).fold[Option[String]](Some(s"""Unresolvable reference: Name ref="$ref">${name.text}< """)) { named =>
            if (named.entityType == reference.entityType) None
            else Some(s"${reference.entityType} reference to ${named.entityType} ${named.name}: $name [$ref]")
          }
        }
      }
    }

    references.flatMap(reference => checkReference(reference))
  }

  def noRef: Seq[EntityReference] = references
    .filter(_.ref.isEmpty)
    .sortBy(reference => Xml.text(reference.name).toLowerCase)

  def toId(id: String): Seq[EntityReference] = references.filter(_.ref.contains(id))
}
