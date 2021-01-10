package org.opentorah.mathjax

import org.opentorah.xml.{Attribute, Dialect, Namespace, PrettyPrinter}

object MathML extends Dialect {

  override val namespace: Namespace = Namespace(uri = "http://www.w3.org/1998/Math/MathML", prefix = "mathml")

  override val mimeType: String = "application/mathml+xml"

  override val prettyPrinter: PrettyPrinter = PrettyPrinter(
    alwaysStackElements = Set("math", "mrow", "mi")
  )

  /**
    * Display mode: inline or block (display math).
    *
    * Note: this attribute is always used within its native MathML.namespace, so I set its namespace to Top.
    */
  final class DisplayAttribute extends Attribute[Boolean]("display", namespace = Namespace.No, default = false) {
    private val inline: String = "inline"
    private val block: String = "block"
    private val values: Set[String] = Set(inline, block)

    override def fromString(value: String): Boolean = {
      require(values.contains(value))
      value == inline
    }

    override def toString(value: Boolean): String = if (value) inline else block
  }

  @SerialVersionUID(1L)
  val displayAttribute: DisplayAttribute = new DisplayAttribute

  val math: String = "math"
  val mrow: String = "mrow"
  val mi: String = "mi"

  def unwrap(mathML: org.w3c.dom.Element): String = mathML
    .getElementsByTagName(MathML.mrow).item(0).asInstanceOf[org.w3c.dom.Element]
    .getElementsByTagName(MathML.mi).item(0).asInstanceOf[org.w3c.dom.Element]
    .getTextContent
}
