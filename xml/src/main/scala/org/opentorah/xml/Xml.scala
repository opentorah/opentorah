package org.opentorah.xml

import org.opentorah.util.Strings
import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.Node

object Xml extends Model[Node] {
  override type Element = scala.xml.Elem
  // Note: some whitespace is packaged in Text, but in a different subclass of Atom[String], so:
  override type Text = scala.xml.Atom[_]

  def transform(xml: Element, transformer: Transformer): Element = {
    val rule: RewriteRule = new RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case element: Element => transformer(element)
        case other => other
      }
    }

    asElement(new RuleTransformer(rule).transform(xml).head)
  }

  def removeNamespace(xml: Element): Element =
    xml.copy(scope = scala.xml.TopScope, child = xml.child.map(removeNamespace))
  def removeNamespace(node: Node): Node = node match {
    case e: Element => e.copy(scope = scala.xml.TopScope, child = e.child.map(removeNamespace))
    case n => n
  }

  def descendants(xml: Node, name: String): Seq[Element] =
    xml.flatMap(_ \\ name).filter(isElement).map(asElement)

  override def toString(node: Node): String = Strings.squashWhitespace {
    node match {
      case elem: Element => (elem.child map (_.text)).mkString(" ")
      case text: Text => text.data.toString
      case special: scala.xml.SpecialNode => Strings.sbToString(special.buildString)
      case node: Node => node.text
    }
  }

  override def isText(node: Node): Boolean = node.isInstanceOf[Text]
  override def asText(node: Node): Text    = node.asInstanceOf[Text]
  override def getText(text: Text): String = text.text

  override def mkText(text: String, seed: Node): Text = mkText(text)
  def mkText(text: String): Text = new scala.xml.Text(text)

  override def isElement(node: Node): Boolean = node.isInstanceOf[Element]
  override def asElement(node: Node): Element = node.asInstanceOf[Element]

  override def getName(element: Element): String = element.label
  override def getPrefix(element: Element): Option[String] = Option(element.prefix)

  override def getAttributes(element: Element, parent: Option[Element]): Seq[Attribute.Value[String]] = {
    val nonXmlnsAttributes: Seq[Attribute.Value[String]] = getNonXmlnsAttributes(element)

    val parentNamespaces = parent.fold[Seq[Namespace]](Seq.empty)(getNamespaces)
    val xmlnsAttributes: Seq[Attribute.Value[String]] = getNamespaces(element).flatMap(namespace =>
      if ((namespace == Namespace.Top) || parentNamespaces.contains(namespace)) None
      else Some(namespace.xmlnsAttribute))

    nonXmlnsAttributes ++ xmlnsAttributes
  }

  def getNonXmlnsAttributes(element: Element): Seq[Attribute.Value[String]] = element.attributes.toSeq
    .filter(_.isInstanceOf[scala.xml.Attribute])
    .map(_.asInstanceOf[scala.xml.Attribute])
    .map(attribute => Attribute(
      name = attribute.key,
      prefix = Option(attribute.pre)
    ).withValue(Option(attribute.value).map(getAttributeValueText)))

  private def getAttributeValueText(value: Seq[Node]): String =
    Strings.sbToString(scala.xml.Utility.sequenceToXML(value, scala.xml.TopScope, _, stripComments = true))

  private def getNamespaces(element: Element): Seq[Namespace] = {
    @scala.annotation.tailrec
    def get(result: Seq[Namespace], namespaceBinding: scala.xml.NamespaceBinding): Seq[Namespace] =
      if (namespaceBinding == null) result
      else get(toNamespace(namespaceBinding) +: result, namespaceBinding.parent)
    get(Seq.empty, element.scope)
  }

  private def toNamespace(namespaceBinding: scala.xml.NamespaceBinding): Namespace = new Namespace(
    prefix = Option(namespaceBinding.prefix).getOrElse(""),
    uri = Option(namespaceBinding.uri).getOrElse("")
  )

  override def getChildren(element: Element): Seq[Node] = element.child

  def mkElement(name: String, attributes: Seq[Attribute.Value[_]], content: Seq[Node]): Element = <elem/>.copy(
    label = name,
    attributes = attributes.foldRight[scala.xml.MetaData](scala.xml.Null){
      case (current, result) => new scala.xml.UnprefixedAttribute(
        current.attribute.name,
        current.valueToString.orNull,
        result
      )},
      child = content
    )
}
