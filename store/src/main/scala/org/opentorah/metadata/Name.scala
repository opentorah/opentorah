package org.opentorah.metadata

import org.opentorah.xml.{Attribute, ContentType, Element, Parser, Text, ToXml}
import scala.xml.Elem

final case class Name(name: String, languageSpec: LanguageSpec) {
  def satisfies(spec: LanguageSpec): Boolean = {
    def satisfies[T](f: LanguageSpec => Option[T]): Boolean = f(spec).isEmpty || (f(languageSpec) == f(spec))

    satisfies(_.language) && satisfies(_.isTransliterated) && satisfies(_.flavour)
  }
}

object Name extends Element[Name](
  elementName = "name",
  contentType = ContentType.Text,
  parser = for {
    n <- Attribute("n").optional
    characters <- Text().optional
    _ <- Parser.check(n.nonEmpty || characters.nonEmpty, "Both 'n' attribute and text are absent.")
    _ <- Parser.check(n.isEmpty || characters.isEmpty, "Both 'n' attribute and text are present.")
    name = n.orElse(characters)
    languageSpec <- LanguageSpec.parser
  } yield new Name(name.get, languageSpec)
) with ToXml[Name] {
  def apply(name: String, language: Language): Name =
    new Name(name, language.toSpec)

  def apply(name: String): Name =
    new Name(name, LanguageSpec.empty)

  override def toXml(name: Name): Elem =
    <name
      lang={name.languageSpec.language.map(_.name).orNull}
      n={name.name}
      transliterated={if (name.languageSpec.isTransliterated.contains(true)) "true" else null}
      flavour={name.languageSpec.flavour.orNull}
    />
}
