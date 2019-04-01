package org.podval.archive19kislev.collector

import scala.xml.Elem
import Xml.Ops

final class Named(
  val id: Option[String],
  val names: Seq[Name]
)

object Named {
  def apply(elem: Elem, nameElemenName: String): Named = new Named(
    id = elem.attributeOption("xml:id"),
    names = elem.elemsFilter(nameElemenName).map(Name.apply)
  )
}
