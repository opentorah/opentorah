package org.digitaljudaica.archive.collector

abstract class HasReferences {
  def references: Seq[Reference]

  def isNames: Boolean

  def collectionReference: String

  def viewer: String

  def name: String

  def url: String
}
