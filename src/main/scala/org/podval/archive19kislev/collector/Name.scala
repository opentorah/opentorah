package org.podval.archive19kislev.collector

import scala.xml.Elem
import Xml.Ops

final class Name(
  val name: String,
  val ref: Option[String],
  val role: Option[String],
  val document: Document
)

object Name {
  private val refPrefix: String = "/ref#"
  def apply(elem: Elem, document: Document): Name = {
    val ref: Option[String] = elem.attributeOption("ref")
    ref.foreach { ref =>
      if (!ref.startsWith(refPrefix)) println(s"!!! Malformed ref in ${document.name}: $ref")
    }
    new Name(
      name = elem.text,
      ref = ref.map(_.substring(refPrefix.length)),
      role = elem.attributeOption("role"),
      document = document
    )
  }
}
