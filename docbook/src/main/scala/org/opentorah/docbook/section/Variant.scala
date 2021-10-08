package org.opentorah.docbook.section

final class Variant(
  val docBook2: DocBook2,
  val name: Option[String]
) derives CanEqual:
  override def equals(other: Any): Boolean =
    val that: Variant = other.asInstanceOf[Variant]
    (this.docBook2 == that.docBook2) && (this.name == that.name)

  def fullName: String = docBook2.name + name.fold("")(name => s"-$name")

  def baseVariant: Option[Variant] = name.map(_ => docBook2.defaultVariant)
