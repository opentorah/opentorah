package org.digitaljudaica.archive.collector.reference

import org.digitaljudaica.archive.collector.CollectionLike

abstract class ReferenceSource(val collection: CollectionLike) {

  def references: Seq[Reference]

  def isNames: Boolean

  def viewer: String

  def name: String

  def url: String

  // TODO unfold
  protected final def bindReferences(storeReferences: Seq[org.digitaljudaica.reference.Reference]): Seq[Reference] =
    storeReferences.map(storeReference => Reference(source = this, storeReference))
}
