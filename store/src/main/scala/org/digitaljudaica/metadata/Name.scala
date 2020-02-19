package org.digitaljudaica.metadata

import cats.implicits._
import org.digitaljudaica.xml.{Parser, Xml}

final case class Name(name: String, languageSpec: LanguageSpec) {
  def satisfies(spec: LanguageSpec): Boolean = {
    def satisfies[T](f: LanguageSpec => Option[T]): Boolean = f(spec).isEmpty || (f(languageSpec) == f(spec))

    satisfies(_.language) && satisfies(_.isTransliterated) && satisfies(_.flavour)
  }
}

object Name {

  val parser: Parser[Name] = for {
    n <- Xml.attribute.optional("n")
    characters <- Xml.text.optional
    _ <- Parser.check(n.nonEmpty || characters.nonEmpty, "Both 'n' attribute and text are absent.")
    _ <- Parser.check(n.isEmpty || characters.isEmpty, "Both 'n' attribute and text are present.")
    name = n.orElse(characters)
    languageSpec <- LanguageSpec.parser
  } yield Name(name.get, languageSpec)
}
