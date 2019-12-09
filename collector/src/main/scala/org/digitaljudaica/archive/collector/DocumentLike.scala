package org.digitaljudaica.archive.collector

import scala.xml.Elem
import Xml.Ops

abstract class DocumentLike(val collection: CollectionLike) {

  def references: Seq[Reference]

  def isNames: Boolean

  def viewer: String

  def name: String

  def url: String

  protected final def parseReferences(xml: Elem): Seq[Reference] =
    for {
      entity <- Entity.values
      elem <- xml.descendants(entity.nameElement)
    } yield Reference(this, elem, entity)
}
