package org.podval.archive19kislev.collector

import scala.xml.Elem
import Xml.Ops

final case class Name(
  name: String,
  id: Option[String],
  ref: Option[String],
  isMalformed: Boolean,
  role: Option[String]
)

object Name {
  private val refPrefix: String = "#"

  def apply(elem: Elem): Name = {
    val (ref: Option[String], isMalformed: Boolean) =
      elem.attributeOption("ref").fold[(Option[String], Boolean)]((None, false)){ ref =>
      if (!ref.startsWith(refPrefix) || ref.contains(" ")) (Some(ref), true)
      else (Some(ref.substring(refPrefix.length)), false)
    }

    new Name(
      name = elem.text,
      id = elem.attributeOption("xml:id"),
      ref = ref,
      isMalformed = isMalformed,
      role = elem.attributeOption("role")
    )
  }
}
