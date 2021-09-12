package org.opentorah.metadata

import org.opentorah.xml.{Attribute, Parsable, Parser, Unparser}

final class LanguageSpec(
  val language: Option[Language],
  val isTransliterated: Option[Boolean],
  val flavour: Option[String]
):
  def toString(number: Int): String = language.fold(number.toString)(_.toString(number))

  def languageName: String = language.get.toLanguageString(using this)

  def dropFlavour: LanguageSpec = copy(flavour = None)

  def dropIsTransliterated: LanguageSpec = copy(isTransliterated = None)

  def dropLanguage: LanguageSpec = copy(language = None)
  
  def copy(
    language: Option[Language] = language,
    isTransliterated: Option[Boolean] = isTransliterated,
    flavour: Option[String] = flavour    
  ): LanguageSpec = new LanguageSpec(
    language,
    isTransliterated,
    flavour
  )

object LanguageSpec extends Parsable[LanguageSpec]:
  val empty: LanguageSpec = new LanguageSpec(None, None, None)

  private val langAttribute: Attribute.Optional[String] = Attribute("lang").optional
  private val transliteratedAttribute: Attribute.Optional[Boolean] = Attribute.BooleanAttribute("transliterated").optional
  private val flavourAttribute: Attribute.Optional[String] = Attribute("flavour").optional

  override protected val parser: Parser[LanguageSpec] = for
    lang: Option[String] <- langAttribute()
    isTransliterated: Option[Boolean] <- transliteratedAttribute()
    flavour: Option[String] <- flavourAttribute()
  yield new LanguageSpec(
    language = lang.map(Language.getForDefaultName),
    isTransliterated = isTransliterated,
    flavour = flavour
  )

  override val unparser: Unparser[LanguageSpec] = Unparser.concat(
    langAttribute(_.language.map(_.name)),
    transliteratedAttribute(_.isTransliterated),
    flavourAttribute(_.flavour)
  )
