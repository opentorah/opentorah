package org.opentorah.math

final case class Input(
  inputType: Input.Type,
  display: Option[Input.Display]
) derives CanEqual

object Input:
  enum Type(val name: String) derives CanEqual:
    case Tex extends Type(name = "tex")
    case AsciiMath extends Type(name = "asciimath")
    case MathML extends Type(name = "mathml")

  // Note: "display" is only important for TeX...
  enum Display(val name: String) derives CanEqual:
    case Inline extends Display("inline")
    case Block  extends Display("block" ) // "display math"

  object Display:
    val default: Display = Display.Block
    def orDefault(value: Option[Display]): Display = value.getOrElse(default)
    def isBlock(value: Option[Display]): Boolean = orDefault(value) == Display.Block
