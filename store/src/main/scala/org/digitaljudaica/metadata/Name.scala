package org.digitaljudaica.metadata

import cats.implicits._
import org.digitaljudaica.xml.Parse.{Parser, check, attribute, characters}

final case class Name(name: String, languageSpec: LanguageSpec) {
  def satisfies(spec: LanguageSpec): Boolean = {
    def satisfies[T](f: LanguageSpec => Option[T]): Boolean = f(spec).isEmpty || (f(languageSpec) == f(spec))

    satisfies(_.language) && satisfies(_.isTransliterated) && satisfies(_.flavour)
  }
}

object Name {

  val parser: Parser[Name] = for {
    n <- attribute("n")
    characters <- characters
    _ <- check(n.nonEmpty || characters.nonEmpty, "Both 'n' requiredAttribute and text are absent.")
    _ <- check(n.isEmpty || characters.isEmpty, "Both 'n' requiredAttribute and text are present.")
    name = n.orElse(characters)
    languageSpec <- LanguageSpec.parser
  } yield Name(name.get, languageSpec)
}
