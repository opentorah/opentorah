package org.opentorah.math

import org.opentorah.xml.Attribute

final case class Input(
  inputType: Input.Type,
  display: Option[Input.Display]
) derives CanEqual

object Input:
  enum Type(val name: String) derives CanEqual:
    case Tex extends Type(name = "tex")
    case AsciiMath extends Type(name = "asciimath")
    case MathML extends Type(name = "mathml")

  object Type:
    @SerialVersionUID(1L)
    val attribute: Attribute[Type] = Attribute.EnumeratedAttribute[Type](
      name = "inputType",
      namespace = DocBookMathFilter.namespace,
      values = Type.values.toIndexedSeq,
      default = Type.MathML,
      getName = _.name
    )

  // Note: "display" is only important for TeX...
  enum Display(val name: String) derives CanEqual:
    case Inline extends Display("inline")
    case Block extends Display("block") // "display math"

  object Display:
    val default: Display = Display.Block
    def orDefault(value: Option[Display]): Display = value.getOrElse(default)
    def isBlock(value: Option[Display]): Boolean = orDefault(value) == Display.Block

    // Note: this attribute is always used within its native MathML.namespace, so I set its namespace to No.
    @SerialVersionUID(1L)
    val attribute: Attribute[Display] = Attribute.EnumeratedAttribute[Display](
      name = "display",
      values = Display.values.toIndexedSeq,
      default = default,
      getName = _.name
    )
