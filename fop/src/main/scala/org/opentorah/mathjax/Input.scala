package org.opentorah.mathjax

import org.opentorah.xml.{Attribute, Namespace}

sealed trait Input {
  def name: String
  def isInline: Option[Boolean]
  def withInline(isInline: Option[Boolean]): Input = this
}

object Input {

  case object Tex extends Input {
    override val name: String = "TeX"
    override def isInline: Option[Boolean] = Some(false)
    override def withInline(isInline: Option[Boolean]): Input =
      if (isInline.contains(true)) TexInline else this
  }

  case object TexInline extends Input {
    override val name: String = "inline-TeX"
    override def isInline: Option[Boolean] = Some(true)
    override def withInline(isInline: Option[Boolean]): Input =
      if (isInline.contains(false)) Tex else this
  }

  case object AsciiMath extends Input {
    override val name: String = "AsciiMath"
    override def isInline: Option[Boolean] = None // same for both
  }

  case object MathML extends Input {
    override val name: String = "MathML"
    override def isInline: Option[Boolean] = None // accepts both
  }

  private val values: Set[Input] = Set(Tex, TexInline, AsciiMath, MathML)

  /**
    * Type of the input: TeX, MathML, AsciiMath.
    */
  @SerialVersionUID(1L)
  case object Attribute extends Attribute[Input]("input") {
    override def namespace: Option[Namespace] = Some(MathJax.Namespace)

    override def toString(value: Input): String = value.name

    override def fromString(value: String): Input =
      values.find(_.name == value).getOrElse(throw new IllegalArgumentException(s"Unknown input type: $value"))

    override def default: Input = Input.MathML
  }
}
