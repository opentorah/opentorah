package org.opentorah.mathjax

final class MathJaxConfiguration(
  val displayMessages: Boolean = false, // determines whether Message.Set() calls are logged
  val displayErrors: Boolean = true, // determines whether error messages are shown on the console
  val undefinedCharError: Boolean = false, // determines whether "unknown characters" (i.e., no glyph in the configured fonts) are saved in the error array
  val extensions: List[String] = List.empty, // a convenience option to add MathJax extensions; example: 'Safe,TeX/noUndefined'
  val fontURL: String = MathJaxConfiguration.fontURL, // for webfont urls in the CSS for HTML output
  val font: String = MathJaxConfiguration.defaultFont,

  // paths: Map[String, String] = Map.empty,  // configures custom path variables (e.g., for third party extensions, cf. test/config-third-party-extensions.js)

  val texDelimiters: Seq[Delimiters] = Seq(new Delimiters("$$", "$$"), new Delimiters("\\[", "\\]")),
  val texInlineDelimiters: Seq[Delimiters] = Seq(new Delimiters("$", "$"), new Delimiters("\\(", "\\)")),
  val asciiMathDelimiters: Seq[Delimiters] = Seq(new Delimiters("`", "`")),
  val processEscapes: Boolean = true
):

  if !MathJaxConfiguration.fonts.contains(font) then
    val knownFonts: String = MathJaxConfiguration.fonts.mkString(", ")
    throw IllegalArgumentException(s"Unknown MathJax font $font; known fonts are: $knownFonts")

    val starts: Seq[String] = (texDelimiters ++ texInlineDelimiters ++ asciiMathDelimiters).map(_.start)
    if starts.toSet.size != starts.size then throw IllegalArgumentException(s"Duplicate start delimiters")

  def allDelimiters: Seq[DelimitersAndInput] =
    def withInput(values: Seq[Delimiters], input: Input): Seq[DelimitersAndInput] =
      for delimiters <- values yield new DelimitersAndInput(delimiters, input)

    val result: Seq[DelimitersAndInput] =
      withInput(texDelimiters, Input.Tex) ++
      withInput(texInlineDelimiters, Input.TexInline) ++
      withInput(asciiMathDelimiters, Input.AsciiMath)

    result.sortWith((l: DelimitersAndInput, r: DelimitersAndInput) => l.start.length > r.start.length)

object MathJaxConfiguration:

  val fontURL: String = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/fonts/HTML-CSS"

  val fonts: Set[String] = Set("TeX", "STIX", "STIX-Web", "Asana-Math", "Neo-Euler", "Gyre-Pagella", "Gyre-Termes", "Latin-Modern")
  // Note that not all mathematical characters are available in all fonts (e.g., Neo-Euler does not include italic
  // characters), so some mathematics may work better in some fonts than in others.
  // The STIX-Web font is the most complete.

  val defaultFont: String = "TeX"

  val inputs: List[String] = List("input/TeX", "input/AsciiMath", "input/MathML")

  val texExtensions: List[String] = List("AMSmath.js", "AMSsymbols.js", "noErrors.js", "noUndefined.js")
