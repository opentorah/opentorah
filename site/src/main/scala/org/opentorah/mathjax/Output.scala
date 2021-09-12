package org.opentorah.mathjax

enum Output(
  val name: String,
  val css: Boolean = false
):
  final def name(isNode: Boolean): String = if isNode then name + "Node" else name

  case MathML extends Output("mml")

  // For SVG, width, height and style attributes are repeated in separate keys of the returned data.
  case Svg extends Output("svg")

  case Html extends Output("html")

  case HtmlWithCss extends Output("html", css = true)
