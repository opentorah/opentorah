package org.podval.calendar.metadata

import scala.xml.Elem

final class Attributes private(attributes: Map[String, String]) {
  private var used: Set[String] = Set.empty

  def get(name: String): Option[String] = {
    used = used + name
    attributes.get(name)
  }

  def close(): Unit = {
    val notAllowed: Set[String] = attributes.keySet -- used
    if (notAllowed.nonEmpty)
      throw new IllegalArgumentException(s"Attributes not allowed: $notAllowed")
  }

  def getInt(name: String): Option[Int] = get(name).map { attribute =>
    val result = attribute.toInt
    require(result > 0, s"Non-positive integer: $result")
    result
  }

  def doGetInt(name: String): Int = getInt(name).get

  def getBoolean(name: String): Option[Boolean] = get(name).map { value =>
    if ((value == "true") || (value == "yes")) true
    else if ((value == "false") || (value == "no")) false
    else throw new IllegalArgumentException("Bad boolean attribute value")
  }

  def doGetBoolean(name: String): Boolean = getBoolean(name).getOrElse(false)
}

object Attributes {
  def apply(element: Elem): Attributes = {
    val attributes = element.attributes.map { metadata =>
      val key = metadata.key
      val value = metadata.value.toString
      key -> value
    }.toMap

    new Attributes(attributes)
  }
}
