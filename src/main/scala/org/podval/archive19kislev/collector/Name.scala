package org.podval.archive19kislev.collector

import scala.xml.Elem
import Xml.Ops

final class Name(
  val name: String,
  val id: Option[String],
  val ref: Option[String],
  val role: Option[String]
)

object Name {
  private val refPrefix: String = "#"

  def apply(elem: Elem): Name = {
    val ref: Option[String] = elem.attributeOption("ref")
    ref.foreach { ref =>
      if (!ref.startsWith(refPrefix)) println(s"!!! Malformed ref: $ref")
//      if (ref.contains(" ")) println(s"!!_ Malformed ref in ${document.name}: $ref")
    }

    new Name(
      name = elem.text,
      id = elem.attributeOption("xml:id"),
      ref = ref.map(_.substring(refPrefix.length)),
      role = elem.attributeOption("role")
    )
  }
}
