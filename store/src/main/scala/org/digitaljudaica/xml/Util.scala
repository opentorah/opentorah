package org.digitaljudaica.xml

import scala.xml.{Elem, Node, TopScope}
import scala.xml.transform.{RewriteRule, RuleTransformer}

object Util {

  private def removeNamespace(node: Node): Node = node match {
    case e: Elem => e.copy(scope = TopScope, child = e.child.map(removeNamespace))
    case n => n
  }

  def removeNamespace(element: Elem): Elem =
    element.copy(scope = TopScope, child = element.child.map(removeNamespace))

  def rewriteElements(xml: Elem, elementRewriter: Elem => Elem): Elem = {
    val rule: RewriteRule = new RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case element: Elem => elementRewriter(element)
        case other => other
      }
    }

    new RuleTransformer(rule).transform(xml).head.asInstanceOf[Elem]
  }
}
