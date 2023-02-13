package org.opentorah.math

import org.opentorah.build.Distribution
import org.opentorah.node.NodeDistribution
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Unparser}

// Note: yes, all the PDF-only fields look weird in the Site configuration;
// they also look weird in the non-PDF formats - but I do not see a clean way to clean this up.

// TODO adjust for versions 2 and 3: fontURL, fonts, font checking, defaultFont.

final class MathConfiguration(
  val mathJaxEnabled     : Option[Boolean], // TODO enable always
  val nodeVersion        : Option[String],
  val useMathJaxV3       : Option[Boolean],
  val font               : Option[String],
  val processEscapes     : Option[Boolean],
  val texDelimiters      : List[Delimiters],
  val texInlineDelimiters: List[Delimiters],
  val asciiMathDelimiters: List[Delimiters]
):
  if font.isDefined && !MathConfiguration.fonts.contains(font.get) then
    val knownFonts: String = MathConfiguration.fonts.mkString(", ")
    throw IllegalArgumentException(s"MathJax: unknown font ${font.get}; known fonts are: $knownFonts")

  val starts: Seq[String] = (texDelimiters ++ texInlineDelimiters ++ asciiMathDelimiters).map(_.start)
  if starts.toSet.size != starts.size then throw IllegalArgumentException(s"Duplicate start delimiters")

  def isEmpty: Boolean =
    mathJaxEnabled.isEmpty && nodeVersion.isEmpty && useMathJaxV3.isEmpty && font.isEmpty && processEscapes.isEmpty &&
    texDelimiters.isEmpty && texInlineDelimiters.isEmpty && asciiMathDelimiters.isEmpty

  def orElse(other: MathConfiguration): MathConfiguration = MathConfiguration(
    mathJaxEnabled      = this.mathJaxEnabled     .orElse(other.mathJaxEnabled      ),
    nodeVersion         = this.nodeVersion        .orElse(other.nodeVersion         ),
    useMathJaxV3        = this.useMathJaxV3       .orElse(other.useMathJaxV3        ),
    font                = this.font               .orElse(other.font                ),
    processEscapes      = this.processEscapes     .orElse(other.processEscapes      ),
    texDelimiters       = orElse(this.texDelimiters      , other.texDelimiters      ),
    texInlineDelimiters = orElse(this.texInlineDelimiters, other.texInlineDelimiters),
    asciiMathDelimiters = orElse(this.asciiMathDelimiters, other.asciiMathDelimiters)
  )

  private def orElse[T](list: List[T], default: List[T]): List[T] = if list.nonEmpty then list else default

  def enableMathJax: Boolean = mathJaxEnabled.contains(true) // TODO in HTML!

  def texConfiguration: Map[String, Matchable] = Map(
    "processEscapes" -> processEscapes.contains(true),
    "inlineMath"     -> Delimiters.json(texInlineDelimiters),
    "displayMath"    -> Delimiters.json(texDelimiters)
  )

  def nodeDistribution: NodeDistribution = NodeDistribution(nodeVersion.get)

object MathConfiguration extends Element[MathConfiguration]("math"):

  private val fonts: Set[String] = Set("TeX", "STIX", "STIX-Web", "Asana-Math", "Neo-Euler", "Gyre-Pagella", "Gyre-Termes", "Latin-Modern")
  // Note that not all mathematical characters are available in all fonts (e.g., Neo-Euler does not include italic
  // characters), so some mathematics may work better in some fonts than in others.
  // The STIX-Web font is the most complete.

  val default: MathConfiguration = new MathConfiguration(
    mathJaxEnabled        = Some(false),
    nodeVersion           = Some(NodeDistribution.versionDefault),
    useMathJaxV3          = Some(true),
    font                  = Some("TeX"),
    processEscapes        = Some(true),
    texDelimiters         = List(Delimiters("$$", "$$") /*, Delimiters("\\[", "\\]")*/),
    texInlineDelimiters   = List(Delimiters("$" , "$" ) /*, Delimiters("\\(", "\\)")*/),
    asciiMathDelimiters   = List(Delimiters("`" , "`" ))
  )

  override def contentParsable: Parsable[MathConfiguration] = new Parsable[MathConfiguration]:
    private val mathJaxEnabledAttribute     : Attribute.Optional[Boolean] = Attribute.BooleanAttribute("mathJaxEnabled"     ).optional
    private val nodeVersionAttribute        : Attribute.Optional[String]  = Attribute.StringAttribute ("nodeVersion"        ).optional
    private val useMathJaxV3Attribute       : Attribute.Optional[Boolean] = Attribute.BooleanAttribute("useMathJaxV3"       ).optional
    private val processEscapesAttribute     : Attribute.Optional[Boolean] = Attribute.BooleanAttribute("processEscapes"     ).optional
    private val fontAttribute               : Attribute.Optional[String]  = Attribute.StringAttribute ("font"               ).optional
    // TODO parse those as nested elements, not as attributes:
    private val texDelimitersAttribute      : Attribute.Optional[String]  = Attribute.StringAttribute ("texDelimiters"      ).optional
    private val texInlineDelimitersAttribute: Attribute.Optional[String]  = Attribute.StringAttribute ("texInlineDelimiters").optional
    private val asciiMathDelimitersAttribute: Attribute.Optional[String]  = Attribute.StringAttribute ("asciiMathDelimiters").optional

    override def parser: Parser[MathConfiguration] = for
      mathJaxEnabled     : Option[Boolean] <- mathJaxEnabledAttribute()
      nodeVersion        : Option[String]  <- nodeVersionAttribute()
      useMathJaxV3       : Option[Boolean] <- useMathJaxV3Attribute()
      font               : Option[String]  <- fontAttribute()
      texDelimiters      : Option[String]  <- texDelimitersAttribute()
      texInlineDelimiters: Option[String]  <- texInlineDelimitersAttribute()
      asciiMathDelimiters: Option[String]  <- asciiMathDelimitersAttribute()
      processEscapes     : Option[Boolean] <- processEscapesAttribute()
    yield MathConfiguration(
      mathJaxEnabled      = mathJaxEnabled,
      nodeVersion         = nodeVersion,
      useMathJaxV3        = useMathJaxV3,
      font                = font,
      processEscapes      = processEscapes,
      texDelimiters       = texDelimiters      .fold(List.empty)(Delimiters.fromStrings),
      texInlineDelimiters = texInlineDelimiters.fold(List.empty)(Delimiters.fromStrings),
      asciiMathDelimiters = asciiMathDelimiters.fold(List.empty)(Delimiters.fromStrings),
    )

    override def unparser: Unparser[MathConfiguration] = Unparser.concat[MathConfiguration](
      mathJaxEnabledAttribute     (_.mathJaxEnabled),
      nodeVersionAttribute        (_.nodeVersion),
      useMathJaxV3Attribute       (_.useMathJaxV3),
      fontAttribute               (_.font),
      processEscapesAttribute     (_.processEscapes),
      texDelimitersAttribute      (math => if math.texDelimiters      .nonEmpty then Some(Delimiters.toStrings(math.texDelimiters      )) else None),
      texInlineDelimitersAttribute(math => if math.texInlineDelimiters.nonEmpty then Some(Delimiters.toStrings(math.texInlineDelimiters)) else None),
      asciiMathDelimitersAttribute(math => if math.asciiMathDelimiters.nonEmpty then Some(Delimiters.toStrings(math.asciiMathDelimiters)) else None),
    )
