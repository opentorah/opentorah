package org.podval.docbook.gradle.mathjax

import Configuration.{Delimiters, DelimitersAndInput}

final case class Configuration(
  displayMessages: Boolean = false,        // determines whether Message.Set() calls are logged
  displayErrors: Boolean = true,           // determines whether error messages are shown on the console
  undefinedCharError: Boolean = false,     // determines whether "unknown characters" (i.e., no glyph in the configured fonts) are saved in the error array
  extensions: List[String] = List.empty,   // a convenience option to add MathJax extensions; example: 'Safe,TeX/noUndefined'
  fontURL: String = Configuration.fontURL, // for webfont urls in the CSS for HTML output
  font: String = Configuration.defaultFont,

  // paths: Map[String, String] = Map.empty,  // configures custom path variables (e.g., for third party extensions, cf. test/config-third-party-extensions.js)

  texDelimiters: Seq[Delimiters] = Seq(new Delimiters("$$", "$$"), new Delimiters("\\[", "\\]")),
  texInlineDelimiters: Seq[Delimiters] = Seq(new Delimiters("$", "$"), new Delimiters("\\(", "\\)")),
  asciiMathDelimiters: Seq[Delimiters] = Seq(new Delimiters("`", "`"))
) {

  if (!Configuration.fonts.contains(font)) {
    val knownFonts: String = Configuration.fonts.mkString(", ")
    throw new IllegalArgumentException(s"Unknown MathJax font $font; known fonts are: $knownFonts")
  }

  {
    val starts: Seq[String] = (texDelimiters ++ texInlineDelimiters ++ asciiMathDelimiters).map(_.start)
    if (starts.toSet.size != starts.size) throw new IllegalArgumentException(s"Duplicate start delimiters")
  }

  def toMap: Map[String, Any] = Map(
    "displayMessages"     -> displayMessages,
    "displayErrors"       -> displayErrors,
    "undefinedCharError"  -> undefinedCharError,
    "extensions"          -> extensions.mkString(","),
    "fontURL"             -> fontURL,
    // standard MathJax configuration options; see https://docs.mathjax.org for more detail
    "MathJax" -> Map(
      "jax" -> List("input/TeX", "input/MathML", "input/AsciiMath", "output/SVG"),
      // matchFontHeight: true
      // mtextFontInherit: false
      // scale: 100
      // minScaleAdjust: 50
      "SVG" -> Map("font" -> font)
    )
  )

  def allDelimiters: Seq[DelimitersAndInput] = {
    def withInput(values: Seq[Delimiters], input: Input): Seq[DelimitersAndInput] =
      for (delimiters <- values) yield new DelimitersAndInput(delimiters, input)

    val result: Seq[DelimitersAndInput] =
      withInput(texDelimiters, Input.Tex) ++
      withInput(texInlineDelimiters, Input.TexInline) ++
      withInput(asciiMathDelimiters, Input.AsciiMath)

    result.sortWith((l: DelimitersAndInput, r: DelimitersAndInput) => l.start.length > r.start.length)
  }
}

object Configuration {
  final class Delimiters(val start: String, val end: String)

  final class DelimitersAndInput(val delimiters: Delimiters, val input: Input) {
    def start: String = delimiters.start
    def end: String = delimiters.end
  }

  val fontURL: String = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/fonts/HTML-CSS"

  val fonts: Set[String] = Set("TeX", "STIX", "STIX-Web", "Asana-Math", "Neo-Euler", "Gyre-Pagella", "Gyre-Termes", "Latin-Modern")
  // Note that not all mathematical characters are available in all fonts (e.g., Neo-Euler does not include italic
  // characters), so some mathematics may work better in some fonts than in others.
  // The STIX-Web font is the most complete.

  val defaultFont: String = "TeX"
}
