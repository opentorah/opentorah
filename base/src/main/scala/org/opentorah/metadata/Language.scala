package org.opentorah.metadata

import org.opentorah.xml.{Attribute, Parsable, Parser, Unparser}

enum Language(code: String) extends
  Named.ByLoader[Language](loader = Language, nameOverride = Some(code)),
  HasName.Enum derives CanEqual:

  final def toSpec: Language.Spec = new Language.Spec(language = Some(this), isTransliterated = None, flavour = None)

  def numberToString(number: Int): String = number.toString

  case English    extends Language("en")
  case Russian    extends Language("ru")
  case Polish     extends Language("pl")
  case French     extends Language("fr")
  case German     extends Language("de")
  case Lithuanian extends Language("lt")
  case Hebrew     extends Language("he"), Language.Hebrew

object Language extends Names.Loader[Language], HasValues.FindByDefaultName[Language], HasValues.FindByName[Language]:
  override val valuesSeq: Seq[Language] = values.toIndexedSeq

  trait ToString:
    final override def toString: String = toLanguageString(using Language.Spec.empty)
    def toLanguageString(using spec: Language.Spec): String

  final class Spec(
    val language: Option[Language],
    val isTransliterated: Option[Boolean],
    val flavour: Option[String]
  ):
    def isEmpty: Boolean = language.isEmpty && isTransliterated.isEmpty && flavour.isEmpty

    def copy(
      language: Option[Language] = language,
      isTransliterated: Option[Boolean] = isTransliterated,
      flavour: Option[String] = flavour
    ): Spec = new Spec(
      language,
      isTransliterated,
      flavour
    )

    def languageName: String = language.get.toLanguageString(using this)

    def toString(number: Int): String = language.fold(number.toString)(_.numberToString(number))

    def dropFlavour: Spec = copy(flavour = None)

    def dropIsTransliterated: Spec = copy(isTransliterated = None)

    def dropLanguage: Spec = copy(language = None)

  object Spec extends Parsable[Spec]:
    val empty: Spec = new Spec(None, None, None)

    private val langAttribute: Attribute.Optional[String] = Attribute("lang").optional
    private val transliteratedAttribute: Attribute.Optional[Boolean] = Attribute.BooleanAttribute("transliterated").optional
    private val flavourAttribute: Attribute.Optional[String] = Attribute("flavour").optional

    override protected val parser: Parser[Spec] = for
      lang: Option[String] <- langAttribute()
      isTransliterated: Option[Boolean] <- transliteratedAttribute()
      flavour: Option[String] <- flavourAttribute()
    yield new Spec(
      language = lang.map(Language.getForDefaultName),
      isTransliterated = isTransliterated,
      flavour = flavour
    )

    override val unparser: Unparser[Spec] = Unparser.concat(
      langAttribute(_.language.map(_.name)),
      transliteratedAttribute(_.isTransliterated),
      flavourAttribute(_.flavour)
    )

  trait Hebrew:
    self: Language =>

    val MAQAF: Char       = '־'
    val PASEQ: Char       = '׀'
    val SOF_PASUQ: Char   = '׃'

    private val units: List[Char] = "אבגדהוזחט".toList
    private val decades: List[Char] = "יכלמנסעפצ".toList
    private val hundreds: List[Char] = "קרש".toList

    override def numberToString(number: Int): String =
      // to display 0 as empty string :)      require(number > 0)
      require(number <= 10000)

      val result: StringBuilder = new StringBuilder
      var remainder: Int = number

      def step(condition: Int => Boolean, append: Int => String, modify: Int => Int): Unit =
        if condition(remainder) then
          result.append(append(remainder))
          remainder = modify(remainder)

      def digit(num: Int, digits: List[Char]): Unit =
        step(_ >= num, n => digits((n / num) - 1).toString, _ % num)

      val addThousandsSeparator: Boolean = remainder >= 1000
      digit(1000, units)
      if addThousandsSeparator then result.append("׳")
      step(_ >= 900, _ => "תת", _ - 800)
      step(_ >= 500, _ => "ת", _ - 400)
      digit(100, hundreds)

      if remainder == 15 then result.append("טו") else
      if remainder == 16 then result.append("טז") else
        digit(10, decades)
        digit(1, units)

      result.toString

    def numberFromString(string: String): Option[Int] =
      var failed: Boolean = false
      var result: Int = 0
      var remainder: String = string

      def addAndDrop(add: Int, drop: Int): Unit =
        result = result + add
        remainder = remainder.drop(drop)

      def digit(digits: List[Char], multiplier: Int): Unit = if remainder.nonEmpty then
        val n: Int = digits.indexOf(remainder.head)
        if n >= 0 then addAndDrop(multiplier*(n+1), 1)

      if remainder.length > 1 && remainder.charAt(1) == '׳' then
        val n: Int = units.indexOf(remainder.head)
        if n < 0 then failed = true
        addAndDrop(1000*(n+1), 2)

      if remainder.startsWith("תת") then addAndDrop(800, 2)
      else if remainder.startsWith("ת") then addAndDrop(400, 1)

      digit(hundreds, 100)

      if remainder == "טו" then addAndDrop(15, 2)
      else if remainder == "טז" then addAndDrop(16, 2)
      else
        digit(decades, 10)
        digit(units, 1)

      if remainder.nonEmpty then failed = true

      if failed then None else Some(result)


