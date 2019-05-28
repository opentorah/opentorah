package org.podval.docbook.gradle.mathjax

import scala.collection.JavaConverters._

final case class MathJaxConfiguration(
  displayMessages: Boolean = false,        // determines whether Message.Set() calls are logged
  displayErrors: Boolean = true,           // determines whether error messages are shown on the console
  undefinedCharError: Boolean = false,     // determines whether "unknown characters" (i.e., no glyph in the configured fonts) are saved in the error array
  extensions: String = "",                 // a convenience option to add MathJax extensions; example: 'Safe,TeX/noUndefined'
  fontURL: String = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/fonts/HTML-CSS", // for webfont urls in the CSS for HTML output
  font: String = "TeX"                     // possible values are TeX, STIX, STIX-Web, Asana-Math, Neo-Euler, Gyre-Pagella,
  // Gyre-Termes and Latin-Modern. Note that not all mathematical characters
  // are available in all fonts (e.g., Neo-Euler does not include italic
  // characters), so some mathematics may work better in some fonts than in
  // others. The STIX-Web font is the most complete.

  // paths: Map[String, String] = Map.empty,  // configures custom path variables (e.g., for third party extensions, cf. test/config-third-party-extensions.js)
) {
  def toMap: java.util.Map[String, Any] = Map(
    "displayMessages"     -> displayMessages,
    "displayErrors"       -> displayErrors,
    "undefinedCharError"  -> undefinedCharError,
    "extensions"          -> extensions,
    "fontURL"             -> fontURL,
    // standard MathJax configuration options; see https://docs.mathjax.org for more detail
    "MathJax" -> Map(
      "jax" -> List("input/TeX", "input/MathML", "input/AsciiMath", "output/SVG").asJava,
      // TODO see https://github.com/mathjax/MathJax/blob/master/unpacked/jax/output/SVG/config.js
      // matchFontHeight: true
      // mtextFontInherit: false
      // scale: 100
      // minScaleAdjust: 50
      "SVG" -> Map("font" -> font).asJava
    ).asJava
  ).asJava

  def start(chars: String): Option[(MathJaxConfiguration.Mode, Int)] = {
    val starts: Seq[(MathJaxConfiguration.Mode, Int)] = for {
      mode <- MathJaxConfiguration.modes
      index = mode.findStart(chars)
      if index.isDefined
    } yield mode -> index.get

    starts.sortBy(_._2).headOption
  }
}

object MathJaxConfiguration {

  sealed trait Mode {
    def start: String
    def end: String
    def processEscapes: Boolean = false
    def isInline: Option[Boolean]
    def input: MathJax.Input = MathJax.Tex

    final def findStart(chars: String): Option[Int] = findUnquoted(start, processEscapes, chars)
    final def findEnd(chars: String): Option[Int] = findUnquoted(end, processEscapes, chars)
  }

  case object TeX extends Mode {
    def start: String = "$$"
    def end: String = "$$"
    def isInline: Option[Boolean] = Some(false)
  }

  case object LaTeX extends Mode {
    def start: String = "\\["
    def end: String = "\\]"
    def isInline: Option[Boolean] = Some(false)
  }

  case object TeXInline extends Mode {
    def start: String = "$"
    def end: String = "$"
    def isInline: Option[Boolean] = Some(true)
  }

  case object LaTeXInline extends Mode {
    def start: String = "\\("
    def end: String = "\\)"
    def isInline: Option[Boolean] = Some(true)
  }

  case object AsciiMath extends Mode {
    def start: String = "`"
    def end: String = "`"
    def isInline: Option[Boolean] = None
    override def input: MathJax.Input = MathJax.AsciiMath
  }

  private val modes: Seq[Mode] = Seq(TeX, TeXInline, LaTeX, LaTeXInline, AsciiMath)

  private def findUnquoted(what: String, processEscapes: Boolean, chars: String): Option[Int] = {
    val index: Int = chars.indexOf(what)
    if (index == -1) None
    else if (index == 0) Some(index)
    else if (processEscapes && chars.charAt(index-1) == '\\') None
    else Some(index)
  }
}
