package org.opentorah.docbook

trait HasName:
  def name: String

object HasName:
  def forName[T <: HasName](name: String, all: List[T], what: String): T =
    all.find(_.name.equalsIgnoreCase(name)).getOrElse {
      val known: String = (for section <- all yield "\"" + section.name +"\"").mkString(", ")
      throw IllegalArgumentException(s"Unknown $what: $name; known: $known")
    }
