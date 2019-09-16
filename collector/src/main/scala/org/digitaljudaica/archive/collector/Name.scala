package org.digitaljudaica.archive.collector

import scala.xml.Elem
import Xml.Ops

final class Name(
  val document: DocumentLike,
  val name: String,
  val id: Option[String],
  val ref: Option[String],
  val isMalformed: Boolean,
  val role: Option[String]
) {
  override def toString: String = document.toString

  def isMissing: Boolean = ref.isEmpty

  def isResolvable: Boolean = !isMissing && !isMalformed

  def display: String =
    if (isMissing) s"Name>$name</"
    else if (isMalformed) s"""ref="${ref.get}" """
    else s"""ref="#${ref.get}" """
}

object Name {
  private val refPrefix: String = "#"

  def apply(document: DocumentLike, elem: Elem): Name = {
    val (ref: Option[String], isMalformed: Boolean) =
      elem.attributeOption("ref").fold[(Option[String], Boolean)]((None, false)){ ref =>
        val (refResult: String, isMalformed: Boolean) =
          if (!ref.startsWith(refPrefix) || ref.contains(" ")) (ref, true)
          else (ref.substring(refPrefix.length), false)
        (Some(refResult), isMalformed)
      }

    new Name(
      document = document,
      name = elem.text,
      id = elem.attributeOption("xml:id"),
      ref = ref,
      isMalformed = isMalformed,
      role = elem.attributeOption("role")
    )
  }
}
