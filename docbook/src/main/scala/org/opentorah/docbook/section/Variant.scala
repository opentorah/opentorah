package org.opentorah.docbook.section

case class Variant(
  docBook2: DocBook2,
  name: Option[String]
) {
  def fullName: String = docBook2.name + name.fold("")(name => s"-$name")
}
