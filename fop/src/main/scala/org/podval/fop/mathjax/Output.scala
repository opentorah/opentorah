package org.podval.fop.mathjax

sealed trait Output {
  def name: String
  final def name(isNode: Boolean): String = if (isNode) name + "Node" else name
  def css: Boolean = false
}

object Output {

  case object MathML extends Output {
    override val name: String = "mml"
  }

  // For SVG, width, height and style attributes are repeated in separate keys of the returned data.
  case object Svg extends Output {
    override val name: String = "svg"
  }

  case object Html extends Output {
    override val name: String = "html"
  }

  case object HtmlWithCss extends Output {
    override val name: String = "html"
    override val css: Boolean = true
  }
}
