package org.opentorah.xml

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Atom, Elem, Node, TopScope}

object Xml {

  val header: String    = """<?xml version="1.0" encoding="UTF-8"?>"""
  val header16: String = """<?xml version="1.0" encoding="UTF-16"?>"""

  type Transformer = Elem => Elem

  def transform(xml: Elem, transformer: Transformer): Elem = {
    val rule: RewriteRule = new RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case element: Elem => transformer(element)
        case other => other
      }
    }

    new RuleTransformer(rule).transform(xml).head.asInstanceOf[Elem]
  }

  def removeNamespace(xml: Elem): Elem =
    xml.copy(scope = TopScope, child = xml.child.map(removeNamespace))

  def removeNamespace(node: Node): Node = node match {
    case e: Elem => e.copy(scope = TopScope, child = e.child.map(removeNamespace))
    case n => n
  }

  def descendants(xml: Node, name: String): Seq[Elem] =
    xml.flatMap(_ \\ name).filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])

  def isWhitespace(node: Node): Boolean =
    isAtom(node) && node.text.trim.isEmpty

  def isElement(node: Node): Boolean =
    node.isInstanceOf[Elem]

  def isAtom(node: Node): Boolean =
    node.isInstanceOf[Atom[_]]

  def isText(node: Node): Boolean =
    node.isInstanceOf[scala.xml.Text]

  def textNode(text: String): Node = new scala.xml.Text(text)

  def toString(nodes: Seq[Node]): String = nodes.map(toString).mkString("")
  def toString(node: Node): String = {
    val result = node match {
      case elem: Elem => (elem.child map (_.text)).mkString(" ")
      case node: Node => node.text
    }
    result
      .replace('\n', ' ')
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
      .replace("  ", " ")
  }
}
