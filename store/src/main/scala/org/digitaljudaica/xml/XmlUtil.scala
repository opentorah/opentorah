package org.digitaljudaica.xml

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Atom, Elem, Node, Text, TopScope}

object XmlUtil {

  def removeNamespace(xml: Elem): Elem =
    xml.copy(scope = TopScope, child = xml.child.map(removeNamespace))

  def removeNamespace(node: Node): Node = node match {
    case e: Elem => e.copy(scope = TopScope, child = e.child.map(removeNamespace))
    case n => n
  }

  def rewriteElements(xml: Elem, elementRewriter: Elem => Elem): Elem = {
    val rule: RewriteRule = new RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case element: Elem => elementRewriter(element)
        case other => other
      }
    }

    new RuleTransformer(rule).transform(xml).head.asInstanceOf[Elem]
  }

  def descendants(xml: Node, name: String): Seq[Elem] =
    xml.flatMap(_ \\ name).filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])

  def spacedText(node: Node): String = {
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

  def dropWhitespace(nodes: Seq[Node]): Seq[Node] =
    nodes.dropWhile(isWhitespace)

  def isWhitespace(node: Node): Boolean =
    isAtom(node) && node.text.trim.isEmpty

  def isElement(node: Node): Boolean =
    node.isInstanceOf[Elem]

  def isAtom(node: Node): Boolean =
    node.isInstanceOf[Atom[_]]

  def isText(node: Node): Boolean =
    node.isInstanceOf[Text]
}
