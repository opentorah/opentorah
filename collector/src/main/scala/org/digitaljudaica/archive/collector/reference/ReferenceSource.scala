package org.digitaljudaica.archive.collector.reference

import org.digitaljudaica.archive.collector.CollectionLike

abstract class ReferenceSource(val collection: CollectionLike) {

  def references: Seq[Reference]

  def isNames: Boolean

  def viewer: String

  def name: String

  def url: String

  protected final def bindReferences(teiReferences: Seq[org.digitaljudaica.reference.Reference]): Seq[Reference] =
    teiReferences.map(teiReference => Reference(this, teiReference))
}
