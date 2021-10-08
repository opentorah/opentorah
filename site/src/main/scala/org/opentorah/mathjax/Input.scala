package org.opentorah.mathjax

enum Input(
  val name: String,
  val isInline: Option[Boolean]
) derives CanEqual:
  case Tex       extends Input(name = "TeX"       , isInline = Some(false))
  case TexInline extends Input(name = "inline-TeX", isInline = Some(true))
  case AsciiMath extends Input(name = "AsciiMath" , isInline = None) // same for both
  case MathML    extends Input(name = "MathML"    , isInline = None) // accepts both

object Input:
  def withInline(input: Input, isInline: Option[Boolean]): Input = input match
    case Tex       if isInline == TexInline.isInline => TexInline
    case TexInline if isInline == Tex      .isInline => Tex
    case _ => input
