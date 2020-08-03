package org.opentorah.metadata

import org.opentorah.xml.{Antiparser, Attribute, Parser}

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

object LanguageSpec {
  val empty: LanguageSpec = LanguageSpec(None, None, None)

  def apply(language: Language): LanguageSpec =
    new LanguageSpec(language = Some(language), isTransliterated = None, flavour = None)

  def apply(language: Language, isTransliterated: Boolean): LanguageSpec =
    new LanguageSpec(language = Some(language), isTransliterated = Some(isTransliterated), flavour = None)

  def apply(language: Language, isTransliterated: Boolean, flavour: String): LanguageSpec =
    new LanguageSpec(language = Some(language), isTransliterated = Some(isTransliterated), flavour = Some(flavour))

  private val langAttribute: Attribute[String] = Attribute("lang")
  private val transliteratedAttribute: Attribute.BooleanAttribute = Attribute.BooleanAttribute("transliterated")
  private val flavourAttribute: Attribute[String] = Attribute("flavour")

  val parser: Parser[LanguageSpec] = for {
    lang <- langAttribute.optional
    isTransliterated <- transliteratedAttribute.optional
    flavour <- flavourAttribute.optional
  } yield LanguageSpec(
    language = lang.map(Language.getForDefaultName),
    isTransliterated = isTransliterated,
    flavour = flavour
  )

  // TODO inherit from ToXml?
  val antiparser: Antiparser[LanguageSpec] = Antiparser(
    langAttribute.toAntiparserOption.premap(_.language.map(_.name)),
    transliteratedAttribute.toAntiparserNonDefaultOption.premap(_.isTransliterated),
    flavourAttribute.toAntiparserOption.premap(_.flavour)
  )
}
