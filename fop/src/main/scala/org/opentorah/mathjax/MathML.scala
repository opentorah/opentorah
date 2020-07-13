package org.opentorah.mathjax

import org.opentorah.xml.{Attribute, Namespace}
import org.w3c.dom.{Document, Element}

object MathML {

  // Note: only MathMLObj.getNormalNamespacePrefix() needs the prefix;
  // everywhere else default mapping is assumed.
  object Namespace extends Namespace(
    uri = "http://www.w3.org/1998/Math/MathML",
    prefix = "mathml"
  )

  /**
    * Display mode: inline or block (display math).
    */
  @SerialVersionUID(1L)
  case object DisplayAttribute extends Attribute[Boolean]("display") {
    private val inline: String = "inline"
    private val block: String = "block"
    private val values: Set[String] = Set(inline, block)

    override def namespace: Option[Namespace] = Some(Namespace)

    override def fromString(value: String): Boolean = {
      require(values.contains(value))
      value == inline
    }

    override def toString(value: Boolean): String = if (value) inline else block

    override def default: Boolean = false
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
