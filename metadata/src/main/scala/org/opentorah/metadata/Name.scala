package org.opentorah.metadata

import org.opentorah.xml.{Attribute, ContentType, Element, Parser, Text}
import scala.xml.Elem

final case class Name(name: String, languageSpec: LanguageSpec) {
  def satisfies(spec: LanguageSpec): Boolean = {
    def satisfies[T](f: LanguageSpec => Option[T]): Boolean = f(spec).isEmpty || (f(languageSpec) == f(spec))

    satisfies(_.language) && satisfies(_.isTransliterated) && satisfies(_.flavour)
  }
}

object Name extends Element.WithToXml[Name]("name") {
  private val nAttribute: Attribute[String] = Attribute("n")

  override protected def contentType: ContentType = ContentType.Characters

  override protected def parser: Parser[Name] = for {
    n <- nAttribute.optional
    characters <- Text().optional
    _ <- Parser.check(n.nonEmpty || characters.nonEmpty, "Both 'n' attribute and text are absent.")
    _ <- Parser.check(n.isEmpty || characters.isEmpty, "Both 'n' attribute and text are present.")
    name = n.orElse(characters)
    languageSpec <- LanguageSpec.parser
  } yield new Name(name.get, languageSpec)

  def apply(name: String, language: Language): Name =
    new Name(name, language.toSpec)

  def apply(name: String): Name =
    new Name(name, LanguageSpec.empty)

  override protected def attributes(name: Name): Seq[Attribute.Value[_]] =
    Seq(nAttribute.withValue(name.name)) ++
    LanguageSpec.attributes(name.languageSpec)

  override protected def content(value: Name): Seq[Elem] = Seq.empty
}
