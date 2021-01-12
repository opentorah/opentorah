package org.opentorah.metadata

import org.opentorah.xml.{Unparser, Attribute, Parsable, Parser}

final case class LanguageSpec(
  language: Option[Language],
  isTransliterated: Option[Boolean],
  flavour: Option[String]
) {
  def toString(number: Int): String = language.fold(number.toString)(_.toString(number))

  def languageName: String = language.get.toLanguageString(this)

  def dropFlavour: LanguageSpec = copy(flavour = None)
  def dropIsTransliterated: LanguageSpec = copy(isTransliterated = None)
  def dropLanguage: LanguageSpec = copy(language = None)
}

object LanguageSpec extends Parsable[LanguageSpec] {
  val empty: LanguageSpec = LanguageSpec(None, None, None)

  def apply(language: Language): LanguageSpec =
    new LanguageSpec(language = Some(language), isTransliterated = None, flavour = None)

  def apply(language: Language, isTransliterated: Boolean): LanguageSpec =
    new LanguageSpec(language = Some(language), isTransliterated = Some(isTransliterated), flavour = None)

  def apply(language: Language, isTransliterated: Boolean, flavour: String): LanguageSpec =
    new LanguageSpec(language = Some(language), isTransliterated = Some(isTransliterated), flavour = Some(flavour))

  private val langAttribute: Attribute.Optional[String] = Attribute("lang").optional
  private val transliteratedAttribute: Attribute.Optional[Boolean] = new Attribute.BooleanAttribute("transliterated").optional
  private val flavourAttribute: Attribute.Optional[String] = Attribute("flavour").optional

  override protected val parser: Parser[LanguageSpec] = for {
    lang <- langAttribute()
    isTransliterated <- transliteratedAttribute()
    flavour <- flavourAttribute()
  } yield LanguageSpec(
    language = lang.map(Language.getForDefaultName),
    isTransliterated = isTransliterated,
    flavour = flavour
  )

  override val unparser: Unparser[LanguageSpec] = Unparser.concat(
    langAttribute(_.language.map(_.name)),
    transliteratedAttribute(_.isTransliterated),
    flavourAttribute(_.flavour)
  )
}
