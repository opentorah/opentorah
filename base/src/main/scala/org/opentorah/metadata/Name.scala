package org.opentorah.metadata

import org.opentorah.util.Effects
import org.opentorah.xml.{Attribute, Element, Parsable, Parser, Text, Unparser}

final class Name(val name: String, val languageSpec: LanguageSpec):
  def satisfies(spec: LanguageSpec): Boolean =
    def satisfies[T](f: LanguageSpec => Option[T]): Boolean = f(spec).isEmpty || (f(languageSpec) == f(spec))
    satisfies(_.language) && satisfies(_.isTransliterated) && satisfies(_.flavour)

  def withNumber(number: Int): Name = withSuffix(toString(number))

  def withNumbers(from: Int, to: Int): Name = withSuffix(toString(from) + "-" + toString(to))

  def toString(number: Int): String = languageSpec.toString(number)

  def withSuffix(suffix: String): Name = Name(
    name = name + " " + suffix,
    languageSpec = languageSpec
  )

object Name extends Element[Name]("name"):
  private val nAttribute: Attribute[String] = Attribute("n")

  override def contentType: Element.ContentType = Element.ContentType.Characters

  override def contentParsable: Parsable[Name] = new Parsable[Name]:
    override def parser: Parser[Name] = for
      n: Option[String] <- nAttribute.optional()
      characters: Option[String] <- Text().optional()
      _ <- Effects.check(n.nonEmpty || characters.nonEmpty, "Both 'n' attribute and text are absent.")
      _ <- Effects.check(n.isEmpty  || characters.isEmpty, "Both 'n' attribute and text are present.")
      name: Option[String] = n.orElse(characters)
      languageSpec: LanguageSpec <- LanguageSpec()
    yield Name(
      name = name.get,
      languageSpec = languageSpec
    )

    override val unparser: Unparser[Name] = Unparser.concat(
      nAttribute.required(_.name),
      LanguageSpec(_.languageSpec)
    )

