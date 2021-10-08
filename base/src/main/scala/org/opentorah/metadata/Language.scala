package org.opentorah.metadata

import org.opentorah.xml.{Attribute, Parsable, Parser, Unparser}

enum Language(code: String)
  extends Named.ByLoader[Language](loader = Language, nameOverride = Some(code)), HasName.Enum derives CanEqual:
  def toSpec: Language.Spec = new Language.Spec(language = Some(this), isTransliterated = None, flavour = None)

  def toString(number: Int): String =
    if this == Hebrew then org.opentorah.metadata.Hebrew.toString(number) else number.toString

  case English    extends Language("en")
  case Russian    extends Language("ru")
  case Polish     extends Language("pl")
  case French     extends Language("fr")
  case German     extends Language("de")
  case Lithuanian extends Language("lt")
  case Hebrew     extends Language("he")

object Language extends Names.Loader[Language], HasValues.FindByDefaultName[Language], HasValues.FindByName[Language]:
  trait ToString:
    final override def toString: String = toLanguageString(using Language.Spec.empty)
    def toLanguageString(using spec: Language.Spec): String

  override val valuesSeq: Seq[Language] = values.toIndexedSeq

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

    def toString(number: Int): String = language.fold(number.toString)(_.toString(number))

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