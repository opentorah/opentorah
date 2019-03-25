package org.podval.archive19kislev.collector

final case class Link(
  text: String,
  url: String,
  cssClass: Option[String] = None
) {
  def toMarkdown: String =
    if (cssClass.isEmpty) s"[$text]($url)"
    else s"""<a class="${cssClass.get}" href="$url">$text</a>"""
}
