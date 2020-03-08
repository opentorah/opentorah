package org.opentorah.metadata

import org.opentorah.xml.{Attribute, Parser, Text}

final case class Name(name: String, languageSpec: LanguageSpec) {
  def satisfies(spec: LanguageSpec): Boolean = {
    def satisfies[T](f: LanguageSpec => Option[T]): Boolean = f(spec).isEmpty || (f(languageSpec) == f(spec))

    satisfies(_.language) && satisfies(_.isTransliterated) && satisfies(_.flavour)
  }
}

// TODO add Descriptor
object Name {

  val parser: Parser[Name] = for {
    n <- Attribute("n").optional
    characters <- Text().optional
    _ <- Parser.check(n.nonEmpty || characters.nonEmpty, "Both 'n' attribute and text are absent.")
    _ <- Parser.check(n.isEmpty || characters.isEmpty, "Both 'n' attribute and text are present.")
    name = n.orElse(characters)
    languageSpec <- LanguageSpec.parser
  } yield Name(name.get, languageSpec)
}
