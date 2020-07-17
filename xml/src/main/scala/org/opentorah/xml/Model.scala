package org.opentorah.xml

//import org.w3c.dom.{Node => DomNode}
import scala.xml.{Node => ScalaNode}
import org.opentorah.util.Strings.sbToString

trait Model[N] {
  type NamespaceBinding
  type Element <: N
  type Text <: N
  type Attribute
  type AttributeValue

  case class AttributeDescriptor(
    prefix: Option[String],
    key: String,
    value: Option[AttributeValue]
  )

  def isElement(node: N): Boolean
  def label(node: N): String // TODO getLabel() - for Elements only...
  def isWhitespace(node: N): Boolean
  def isText(node: N): Boolean
  def getText(node: N): String
  def mkText(text: String): Text
  def isAtom(node: N): Boolean
  def topNamespaceBinding: NamespaceBinding
  def getNamespaceBinding(element: Element): NamespaceBinding
  def getNamespaceBindingString(element: Element, namespaceBinding: NamespaceBinding): String
  def getAttributes(element: Element): Seq[AttributeDescriptor]
  def getAttributeValueText(value: AttributeValue): String
  def getChildren(element: Element): Seq[N]
  def getNameString(element: Element): String
}

object Model {

  implicit val scalaModel: Model[ScalaNode] = new Model[ScalaNode] {
    override type NamespaceBinding = scala.xml.NamespaceBinding
    override type Element = scala.xml.Elem
    override type Attribute = scala.xml.MetaData
    override type AttributeValue = Seq[ScalaNode]
    override type Text = scala.xml.Text

    override def isElement(node: ScalaNode): Boolean = Xml.isElement(node)
    override def label(node: ScalaNode): String = node.label
    override def isWhitespace(node: ScalaNode): Boolean = Xml.isWhitespace(node)
    override def isText(node: ScalaNode): Boolean = Xml.isText(node)
    override def getText(node: ScalaNode): String = node.asInstanceOf[Text].data
    override def mkText(text: String): Text = scala.xml.Text(text)
    override def isAtom(node: ScalaNode): Boolean = Xml.isAtom(node)

    override def topNamespaceBinding: NamespaceBinding = scala.xml.TopScope
    override def getNamespaceBinding(element: Element): NamespaceBinding = element.scope

    override def getNamespaceBindingString(element: Element, namespaceBinding: NamespaceBinding): String =
      element.scope.buildString(namespaceBinding).trim

    override def getAttributes(element: Element): Seq[AttributeDescriptor] = {
      element.attributes.toSeq
        .filter(_.isInstanceOf[scala.xml.Attribute]).map(_.asInstanceOf[scala.xml.Attribute]).map { attribute =>
        AttributeDescriptor(
          prefix = Option(attribute.pre),
          key = attribute.key,
          value = Option(attribute.value)
        )
      }
    }

    override def getAttributeValueText(value: Seq[ScalaNode]): String =
      sbToString(scala.xml.Utility.appendQuoted(
        sbToString(scala.xml.Utility.sequenceToXML(value, scala.xml.TopScope, _, stripComments = true)), _))

    override def getChildren(element: Element): Seq[ScalaNode] = element.child

    override def getNameString(element: Element): String = sbToString(element.nameToString)
  }
}
