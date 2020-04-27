package org.opentorah.fop.mathjax

import org.opentorah.fop.xml.Attribute
import org.w3c.dom.{Document, Element}

object MathML {

  // Note: only MathMLObj.getNormalNamespacePrefix() needs the prefix;
  // everywhere else default mapping is assumed.
  object Namespace extends org.opentorah.fop.xml.Namespace(
    uri = "http://www.w3.org/1998/Math/MathML",
    prefix = "mathml"
  )

  /**
    * Display mode: inline or block (display math).
    */
  @SerialVersionUID(1L)
  case object DisplayAttribute extends Attribute[Boolean] {
    private val inline: String = "inline"
    private val block: String = "block"
    private val values: Set[String] = Set(inline, block)

    override def namespace: org.opentorah.fop.xml.Namespace = Namespace
    override def name: String = "display"

    override def fromString(value: String): Boolean = {
      require(values.contains(value))
      value == inline
    }

    override def toString(value: Boolean): String = if (value) inline else block

    def default: Boolean = false
  }

  val mimeType: String = "application/mathml+xml"

  val math: String = "math"
  val mrow: String = "mrow"
  val mi: String = "mi"

  def unwrap(mathMLDocument: Document): String = mathMLDocument.getDocumentElement
    .getElementsByTagName(MathML.mrow).item(0).asInstanceOf[Element]
    .getElementsByTagName(MathML.mi).item(0).asInstanceOf[Element]
    .getTextContent
}
