package org.opentorah.docbook.section

final class Variant(
  val docBook2: DocBook2,
  val name: Option[String]
):
  def fullName: String = docBook2.name + name.fold("")(name => s"-$name")

  def baseVariant: Option[Variant] = name.map(_ => docBook2.defaultVariant)
