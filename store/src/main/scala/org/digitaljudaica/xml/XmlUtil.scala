package org.digitaljudaica.xml

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, Node, TopScope}

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

  // TODO difference?
  def hasAttribute(elem: Elem, name: String, value: String): Boolean =
    elem.attributes.asAttrMap.get(name).contains(value)
  //    def attributeOption(name: String): Option[String] = {
  //      val result: Seq[Node] = elem \ ("@" + name)
  //      if (result.isEmpty) None else Some(result.text)
  //    }
}
