package org.digitaljudaica.xml

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, Node, TopScope}

object Ops {

  def removeNamespace(xml: Elem): Elem =
    xml.copy(scope = TopScope, child = xml.child.map(removeNamespace))

  private def removeNamespace(node: Node): Node = node match {
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

  def descendants(xml: Elem, name: String): Seq[Elem] =
    xml.flatMap(_ \\ name).filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])


  implicit class Ops(elem: Elem) {

    def elemsFilter(name: String): Seq[Elem] = elem.elems.filter(_.label == name)

    // TODO dup!
    def elems: Seq[Elem] = elem.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])

    def elems(name: String): Seq[Elem] = {
      val result = elem.elems
      result.foreach(_.check(name))
      result
    }

    def getAttribute(name: String): String =
      attributeOption(name).getOrElse(throw new NoSuchElementException(s"No requiredAttribute $name"))

    // TODO difference?
    def attributeOption(name: String): Option[String] = elem.attributes.asAttrMap.get(name)
    //    def attributeOption(name: String): Option[String] = {
    //      val result: Seq[Node] = elem \ ("@" + name)
    //      if (result.isEmpty) None else Some(result.text)
    //    }

    def oneChild(name: String): Elem = oneOptionalChild(name, required = true).get
    def optionalChild(name: String): Option[Elem] = oneOptionalChild(name, required = false)

    private[this] def oneOptionalChild(name: String, required: Boolean = true): Option[Elem] = {
      val children = elem \ name

      if (children.size > 1) throw new NoSuchElementException(s"To many children with name '$name'")
      if (required && children.isEmpty) throw new NoSuchElementException(s"No child with name '$name'")

      if (children.isEmpty) None else Some(children.head.asInstanceOf[Elem])
    }

    def check(name: String): Elem = {
      if (elem.label != name) throw new NoSuchElementException(s"Expected name $name but got $elem.label")
      elem
    }
  }
}
