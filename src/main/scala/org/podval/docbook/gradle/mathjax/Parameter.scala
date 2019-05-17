package org.podval.docbook.gradle.mathjax

import org.podval.docbook.gradle.Namespace
import Namespace.MathML
import org.w3c.dom.{Document, Element}
import org.xml.sax.Attributes
import org.xml.sax.helpers.AttributesImpl

// Type-safe XML attribute get/set - for use in DOM and SAX.
// Inspired by net.sourceforge.jeuclid.context.Parameter and friends.
object Parameter {

  sealed trait Parameter[T] {
    def namespace: Namespace = Namespace.MathJax

    def name: String

    final def qName: String = namespace.qName(name)

    def fromString(value: String): T

    def toString(value: T): String

    final def get(document: Document): Option[T] = {
      val element: Element = document.getDocumentElement
      get(element.getAttributeNS(namespace.uri, name))
    }

    final def set(value: T, document: Document): Unit = {
      val element = document.getDocumentElement
      val inDefaultNamespace: Boolean = namespace.is(element.getNamespaceURI)
      if (inDefaultNamespace) element.setAttribute(name, toString(value))
      else {
        // declare the attribute's namespace if it is not declared
        namespace.ensureDeclared(element)

        element.setAttributeNS(namespace.uri, qName, toString(value))
      }
    }

    final def get(attributes: Attributes): Option[T] =
      get(attributes.getValue(namespace.uri, name))

    final def set(defaultNamespace: Namespace, value: T, attributes: AttributesImpl): Unit = {
      // Note: when attribute is added with the default namespace, this is not detected and a new namespace
      // with the same URI gets auto-declared - is this a bug or a feature?
      // Work-around: set the attribute *without* the namespace when I know that it is the default one!
      val inDefaultNamespace: Boolean = namespace.is(defaultNamespace)
      attributes.addAttribute(
        if (inDefaultNamespace) "" else namespace.uri,
        name,
        if (inDefaultNamespace) name else qName,
        "CDATA",
        toString(value)
      )
    }

    private def get(value: String): Option[T] =
      Option(value).filter(_.nonEmpty).map(fromString)
  }

  trait BooleanParameter extends Parameter[Boolean] {
    override def fromString(value: String): Boolean = value.toBoolean
    override def toString(value: Boolean): String = value.toString
  }

  trait StringParameter extends Parameter[String] {
    override def fromString(value: String): String = value
    override def toString(value: String): String = value
  }

  trait FloatParameter extends Parameter[Float] {
    override def fromString(value: String): Float = value.toFloat
    override def toString(value: Float): String = value.toString
  }

  trait IntParameter extends Parameter[Int] {
    override def fromString(value: String): Int = value.toInt
    override def toString(value: Int): String = value.toString
  }

  trait StringListParameter extends Parameter[List[String]] {
    override def fromString(value: String): List[String] = value.split(",").toList.map(_.trim)
    override def toString(value: List[String]): String = value.mkString(",")
  }

  // Typesetting mode: TeX, AsciiMath, MathML.
  @SerialVersionUID(1L)
  case object Mode extends StringParameter {
    override def name: String = "mode"
  }

  // Display mode: inline or block (display math).
  @SerialVersionUID(1L)
  case object Display extends Parameter[Boolean] {
    private val inline: String = "inline"
    private val block: String = "block"
    private val values: Set[String] = Set(inline, block)

    override def namespace: Namespace = MathML
    override def name: String = "display"

    override def fromString(value: String): Boolean = {
      require(values.contains(value))
      value == inline
    }

    override def toString(value: Boolean): String = if (value) inline else block
  }

  // Font size used for the output (mathsize).
  @SerialVersionUID(1L)
  case object FontSize extends FloatParameter {
    override def name: String = "fontSize"
  }
}
