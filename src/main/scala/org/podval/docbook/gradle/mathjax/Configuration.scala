package org.podval.docbook.gradle.mathjax

import Configuration.Delimiters

final case class Configuration(
  displayMessages: Boolean = false,        // determines whether Message.Set() calls are logged
  displayErrors: Boolean = true,           // determines whether error messages are shown on the console
  undefinedCharError: Boolean = false,     // determines whether "unknown characters" (i.e., no glyph in the configured fonts) are saved in the error array
  extensions: String = "",                 // a convenience option to add MathJax extensions; example: 'Safe,TeX/noUndefined'
  fontURL: String = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/fonts/HTML-CSS", // for webfont urls in the CSS for HTML output
  font: String = "TeX",                    // possible values are TeX, STIX, STIX-Web, Asana-Math, Neo-Euler, Gyre-Pagella,
                                           // Gyre-Termes and Latin-Modern. Note that not all mathematical characters
                                           // are available in all fonts (e.g., Neo-Euler does not include italic
                                           // characters), so some mathematics may work better in some fonts than in
                                           // others. The STIX-Web font is the most complete.

  // paths: Map[String, String] = Map.empty,  // configures custom path variables (e.g., for third party extensions, cf. test/config-third-party-extensions.js)

  texDelimiters: Seq[Delimiters] = Seq(new Delimiters("$$", "$$"), new Delimiters("\\[", "\\]")),
  texInlineDelimiters: Seq[Delimiters] = Seq(new Delimiters("$", "$"), new Delimiters("\\(", "\\)")),
  asciiMathDelimiters: Seq[Delimiters] = Seq(new Delimiters("`", "`"))
) {
  def toMap: Map[String, Any] = Map(
    "displayMessages"     -> displayMessages,
    "displayErrors"       -> displayErrors,
    "undefinedCharError"  -> undefinedCharError,
    "extensions"          -> extensions,
    "fontURL"             -> fontURL,
    // standard MathJax configuration options; see https://docs.mathjax.org for more detail
    "MathJax" -> Map(
      "jax" -> List("input/TeX", "input/MathML", "input/AsciiMath", "output/SVG"),
      // TODO see https://github.com/mathjax/MathJax/blob/master/unpacked/jax/output/SVG/config.js
      // matchFontHeight: true
      // mtextFontInherit: false
      // scale: 100
      // minScaleAdjust: 50
      "SVG" -> Map("font" -> font)
    )
  )
}

object Configuration {
  final class Delimiters(val start: String, val end: String)
}
