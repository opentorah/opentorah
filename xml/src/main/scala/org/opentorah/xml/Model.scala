package org.opentorah.xml

import org.w3c.dom.{Node => DomNode}
import scala.xml.{Node => ScalaNode}

trait Model[N] {
  type Text

  def isElement(node: N): Boolean
  def label(node: N): String
  def isWhitespace(node: N): Boolean
  def isText(node: N): Boolean
  def getText(node: N): String
  def isAtom(node: N): Boolean
}

object Model {
  implicit val scalaModel: Model[ScalaNode] = new Model[ScalaNode] {
    override type Text = scala.xml.Text

    override def isElement(node: ScalaNode): Boolean = Xml.isElement(node)
    override def label(node: ScalaNode): String = node.label
    override def isWhitespace(node: ScalaNode): Boolean = Xml.isWhitespace(node)
    override def isText(node: ScalaNode): Boolean = Xml.isText(node)
    override def getText(node: ScalaNode): String = node.asInstanceOf[Text].data
    override def isAtom(node: ScalaNode): Boolean = Xml.isAtom(node)
  }
}