package org.digitaljudaica.archive.collector.reference

import org.digitaljudaica.archive.collector.CollectionLike
import org.digitaljudaica.xml.Ops._
import scala.xml.Elem

abstract class ReferenceSource(val collection: CollectionLike) {

  def references: Seq[Reference]

  def isNames: Boolean

  def viewer: String

  def name: String

  def url: String

  protected final def parseReferences(xml: Elem): Seq[Reference] =
    for {
      entity <- Entity.values
      elem <- xml.descendants(entity.nameElement)
    } yield new Reference(this, entity, elem)
}
