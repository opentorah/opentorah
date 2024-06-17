package org.opentorah.xml

import org.opentorah.util.Strings

type Node = scala.xml.Node

object Node:
  def toString(node: Node): String = Strings.squashWhitespace(node match // TODO why the squash?
    case elem: Element => (elem.child map (_.text)).mkString(" ") // TODO hope this is not used: no tags, no attributes...

    // TODO is the rest any different from _.text?
    case text: Atom => text.data.toString
    case special: scala.xml.SpecialNode => Strings.sbToString(special.buildString)
    case node: Node => node.text
  )

  trait Companion[T]:
    def is(node: Node): Boolean
    def as(node: Node): T
