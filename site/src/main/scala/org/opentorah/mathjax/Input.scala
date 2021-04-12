package org.opentorah.mathjax

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

  val values: Set[Input] = Set(Tex, TexInline, AsciiMath, MathML)
}
