package org.opentorah.xml

import scala.xml.{Elem, Node}

// TODO when Parsable starts carrying the type (for Choice), collapse this:
class RawXml(elementName: String) {

  final class Value(val xml: Seq[Node])

  object parsable extends Element[Value](
    elementName,
    ContentType.Mixed,
    Parser.allNodes.map(new Value(_))
  ) with ToXml[Value] {

    override def toString: String = s"raw element $elementName"

    override def toXml(value: Value): Elem = <elem>{value.xml}</elem>.copy(label = elementName)
  }
}
