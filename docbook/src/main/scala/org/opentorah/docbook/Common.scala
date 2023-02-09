package org.opentorah.docbook

trait Common extends Section, HasName derives CanEqual:
  final override def equals(other: Any): Boolean = other.isInstanceOf[Common] && this.name == other.asInstanceOf[Common].name

  final def fullName: String = s"common-$name"

object Common:
  val all: List[Common] = List(AllCommon, HtmlCommon)

  def forName(name: String): Common = HasName.forName(name, all, "common section")
