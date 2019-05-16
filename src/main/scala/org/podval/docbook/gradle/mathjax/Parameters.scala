package org.podval.docbook.gradle.mathjax

import Parameters.Parameter
import org.w3c.dom.{Document, Element}

// Inspired by net.sourceforge.jeuclid.context.Parameter and friends

final class Parameters extends Serializable {

  private val parameters: collection.mutable.Map[Parameter[_], Any] =
    new collection.mutable.HashMap[Parameter[_], Any]

  def setParameter[T](parameter: Parameter[T], value: T): Unit =
    parameters.put(parameter, value)

  private def setParameterFromString[T](parameter: Parameter[T], value: String): Unit =
    setParameter(parameter, parameter.fromString(value))

  def getParameter[T](parameter: Parameter[T]): T =
    parameters.getOrElse(parameter, parameter.default).asInstanceOf[T]

  def getFontSize: Float = getParameter(Parameters.FontSize)

  def getFontExSize: Int = getFontSize.toInt

  def getMode: String = getParameter(Parameters.Mode)

  def serializeInto(element: Element): Unit =
    for { (parameter, value) <- parameters; if parameter.get(element).isEmpty } parameter.set(value, element)
}

object Parameters {

  val MathJaxNameSpace = "http://podval.org/mathjax/ns/ext"

  val MathJaxAttributePrefix: String = "mathjax"

  sealed trait Parameter[T] {
    def name: String

    final def prefixedName: String = MathJaxAttributePrefix + ":" + name

    def description: String

    def default: T

    def fromString(value: String): T

    def toString(value: T): String

    final def valueToString(value: Any): String = toString(value.asInstanceOf[T])

    def get(element: Element): Option[String] = {
      def empty(value: String): Boolean = (value == null) || value.isEmpty

      // Note: XMLObj.setAttributes() puts an attribute of the parsed MathML 'math' element
      // into a namespace only if the corresponding 'xmlns' attribute precedes it,
      // which for some reason it does not, regardless of how I order the attributes in the
      // TeX wrapper (bug in Xerces? me not understanding what is going on?);
      // so I have to look the 'mode' attribute up both with and *without* the namespace
      // (and thus - the prefix) ...
      Some(element.getAttributeNS(MathJaxNameSpace, prefixedName)).filterNot(empty)
        .orElse(Some(element.getAttribute(name)).filterNot(empty))
    }

    def set(value: Any, element: Element): Unit =
      element.setAttributeNS(MathJaxNameSpace, prefixedName, valueToString(value))
  }

  def apply(document: Document): Parameters = {
    val result: Parameters = new Parameters
    val element: Element = document.getDocumentElement
    for (parameter <- values)
      parameter.get(element).foreach(value => result.setParameterFromString(parameter, value))
    result
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

  @SerialVersionUID(1L)
  case object Mode extends StringParameter {
    override def name: String = "mode"
    override def description: String = "Typesetting mode: TeX, AsciiMath, MathML"
    override def default: String = MathJax.MathML.input
  }

  @SerialVersionUID(1L)
  case object FontSize extends FloatParameter {
    override def name: String = "fontSize"
    override def description: String = "font size used for the output (mathsize)"
    override def default: Float = 12.0f
  }

  @SerialVersionUID(1L)
  case object AntiAlias extends BooleanParameter {
    override def name: String = "antiAlias"
    override def description: String = "anti-alias mode"
    override def default: Boolean = true
  }

  @SerialVersionUID(1L)
  case object Fonts extends StringListParameter {
    override def name: String = "fontsSerif"
    override def description: String = "list of font families for Serif"
    override def default: List[String] = List("Constantina", "Cambria", "Times", "Times New Roman", "Lucida Bright",
      "DejaVu Serif", "DejaVuSerif", "Bitstream Vera Serif", "Luxi Serif", "FreeSerif", "serif")
  }

  private val values: List[Parameter[_]] = List(Mode, FontSize, AntiAlias, Fonts)
}
