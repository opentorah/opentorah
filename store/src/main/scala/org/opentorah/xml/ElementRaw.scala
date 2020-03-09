package org.opentorah.xml

import zio.ZIO
import scala.xml.Elem

class ElementRaw[A](
  elementName: String,
  fromXml: Elem => A
) extends Repeatable.WithElementName[A](elementName) {

  override def toString: String = s"raw element $elementName"

  final override protected def parse(elem: Elem): Parser[A] =
    ZIO.succeed(fromXml(elem))
}

object ElementRaw {

  def apply(elementName: String): Repeatable[Elem] =
    new ElementRaw(elementName, identity)

  def apply[A](elementName: String, fromXml: Elem => A): Repeatable[A] =
    new ElementRaw(elementName, fromXml)
}
