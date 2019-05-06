package org.podval.docbook.gradle.mathjax

import java.awt.Color

import Parameters.Parameter
import org.w3c.dom.Element

// inspired by net.sourceforge.jeuclid.context.Parameter and friends

final class Parameters extends Serializable {

  private val parameters: collection.mutable.Map[Parameter[_], Any] =
    new collection.mutable.HashMap[Parameter[_], Any]

  def setParameter[T](parameter: Parameter[T], value: T): Unit =
    parameters.put(parameter, value)

  def setParameterFromString[T](parameter: Parameter[T], value: String): Unit =
    setParameter(parameter, parameter.fromString(value))

  def getParameter[T](parameter: Parameter[T]): T =
    parameters.getOrElse(parameter, parameter.default).asInstanceOf[T]

  def serializeInto(element: Element): Unit = for ((parameter, value) <- parameters) {
    val localName = parameter.name
    val currentValue: String = element.getAttributeNS(Parameters.AttributeNameSpace, localName)
    val isSet = (currentValue != null) && (currentValue.length != 0)
    if (!isSet) element.setAttributeNS(
      Parameters.AttributeNameSpace,
      Parameters.AttributePrefix + localName,
      parameter.valueToString(value)
    )
  }
}

object Parameters {
  val AttributeNameSpace = "http://podval.org/mathjax/ns/ext"

  val AttributePrefix: String = "mathjax:"

  sealed trait Parameter[T] {
    def nullAllowed: Boolean = false

    def name: String

    def description: String

    def default: T

    def fromString(value: String): T

    def toString(value: T): String

    final def valueToString(value: Any): String = toString(value.asInstanceOf[T])
  }

  def apply(element: Element): Parameters = {
    val result = new Parameters
    for (parameter <- values) {
      val localName = parameter.name
      val value: String = element.getAttributeNS(Parameters.AttributeNameSpace, localName)
      val isSet: Boolean = (value != null) && (value.length != 0)
      if (isSet) result.setParameterFromString(parameter, value)
    }
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

  trait ColorParameter extends Parameter[Color] {
    override def fromString(value: String): Color = ???
    override def toString(value: Color): String = ???
  }

  //  case object Display(EnumTypeWrapper.getInstance(Display.class), false, "display", "display style", Display.BLOCK)

  // You may also want to set ScriptMinSize.
  case object MathSize extends FloatParameter {
    override def name: String = "fontSize"
    override def description: String = "font size used for the output (mathsize)"
    override def default: Float = 12.0f
  }

  case object ScriptMinSize extends FloatParameter {
    override def name: String = "scriptMinSize"
    override def description: String = "font size to be used for smallest script"
    override def default: Float = 8.0f
  }

  case object ScriptSizeMultiplier extends FloatParameter {
    override def name: String = "scriptSizeMult"
    override def description: String = "script size multiplier"
    override def default: Float = 0.71f
  }

  case object ScriptLevel extends IntParameter {
    override def name: String = "scriptLevel"
    override def description: String = "script level"
    override def default: Int = 0
  }

  case object AntiAliasMinSize extends FloatParameter {
    override def name: String = "antiAliasMinSize"
    override def description: String = "minimum font size for which anti-alias is turned on"
    override def default: Float = 10.0f
  }

  case object Debug extends BooleanParameter {
    override def name: String = "debug"
    override def description: String = "debug mode - if on, elements will have borders drawn around them"
    override def default: Boolean = false
  }

  case object AntiAlias extends BooleanParameter {
    override def name: String = "antiAlias"
    override def description: String = "anti-alias mode"
    override def default: Boolean = true
  }

  // Default foreground color (Color). See 3.2.2.2
  case object MathColor extends ColorParameter {
    override def name: String = "foregroundColor"
    override def description: String = "default foreground color (mathcolor)"
    override def default: Color = Color.BLACK
  }

  // Default background color (Color), may be null. See 3.2.2.2
  case object MathBackground extends ColorParameter {
    override def nullAllowed: Boolean = true
    override def name: String = "backgroundColor"
    override def description: String = "default background color (mathbackground)"
    override def default: Color = null
  }

  case object FontsSansSerif extends StringListParameter {
    override def name: String = "fontsSansSerif"
    override def description: String = "list of font families for Sans-Serif"
    override def default: List[String] = List("Verdana", "Helvetica", "Arial", "Arial Unicode MS",
      "Lucida Sans Unicode", "Lucida Sans", "Lucida Grande", "DejaVu Sans", "DejaVuSans", "Bitstream Vera Sans",
      "Luxi Sans", "FreeSans", "sansserif")
  }

  case object FontsSerif extends StringListParameter {
    override def name: String = "fontsSerif"
    override def description: String = "list of font families for Serif"
    override def default: List[String] = List("Constantina", "Cambria", "Times", "Times New Roman", "Lucida Bright",
      "DejaVu Serif", "DejaVuSerif", "Bitstream Vera Serif", "Luxi Serif", "FreeSerif", "serif")
  }

  case object FontsMonospaced extends StringListParameter {
    override def name: String = "fontsMonospaced"
    override def description: String = "list of font families for Monospaced"
    override def default: List[String] = List("Andale Mono", "Courier", "Courier Mono", "Courier New",
      "Lucida Sans Typewriter", "DejaVu Sans Mono", "DejaVuSansMono", "Bitstream Vera Sans Mono", "Luxi Mono",
      "FreeMono", "monospaced")
  }

  case object FontsScript extends StringListParameter {
    override def name: String = "fontsScript"
    override def description: String = "list of font families for Script"
    override def default: List[String] = List("EUSM10", "cmsy10", "Math5", "Mathematica5", "Savoye LET",
      "Brush Script MT", "Zapfino", "Apple Chancery", "Edwardian Script ITC", "Lucida Handwriting", "Monotype Corsiva",
      "Santa Fe LET")
  }

  case object FontsFraktur extends StringListParameter {
    override def name: String = "fontsFraktur"
    override def description: String = "list of font families for Fraktur"
    override def default: List[String] = List("EUFM10", "Mathematica6", "FetteFraktur", "Fette Fraktur",
      "Euclid Fraktur", "Lucida Blackletter", "Blackmoor LET")
  }

  case object FontsDoubleStruck extends StringListParameter {
    override def name: String = "fontsDoublestruck"
    override def description: String = "list of font families for Double-Struck"
    override def default: List[String] = List("MSBM10", "Mathematica7", "Caslon Open Face", "Caslon Openface",
      "Cloister Open Face", "Academy Engraved LET", "Colonna MT", "Imprint MT Shadow")
  }

  case object MfracKeepScriptLevel extends BooleanParameter {
    override def name: String = "mfracKeepScriptLevel"
    override def description: String = "if true, <mfrac> element will NEVER increase children's scriptlevel (in violation of the spec)"
    override def default: Boolean = false
  }

  val values: List[Parameter[_]] = List(
    MathSize
    //ScriptMinSize, ScriptSizeMultiplier, ScriptLevel, AntiAliasMinSize,
    //Debug, AntiAlias,
    //MathColor, MathBackground,
    //FontsSansSerif, FontsSerif, FontsMonospaced, FontsScript, FontsFraktur, FontsDoubleStruck,
    //MfracKeepScriptlevel
  )
}
