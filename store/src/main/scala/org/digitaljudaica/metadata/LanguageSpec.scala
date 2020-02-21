package org.digitaljudaica.metadata

import org.digitaljudaica.xml.{Parser, Xml}

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

  val parser: Parser[LanguageSpec] = for {
    lang <- Xml.attribute.optional("lang")
    isTransliterated <- Xml.attribute.optional.boolean("transliterated")
    flavour <- Xml.attribute.optional("flavour")
  } yield LanguageSpec(
    language = lang.map(Language.getForDefaultName),
    isTransliterated = isTransliterated,
    flavour = flavour
  )
}
