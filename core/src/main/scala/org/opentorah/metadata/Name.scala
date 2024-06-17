package org.opentorah.metadata

import org.opentorah.util.Effects
import org.opentorah.xml.{Attribute, ContentType, ElementTo, Parsable, Parser, Text, Unparser}

final class Name(val name: String, val languageSpec: Language.Spec):
  def satisfies(spec: Language.Spec): Boolean =
    def satisfies[T](f: Language.Spec => Option[T])(using CanEqual[T, T]): Boolean = f(spec).isEmpty || (f(languageSpec) == f(spec))
    satisfies(_.language) && satisfies(_.isTransliterated) && satisfies(_.flavour)

object Name extends ElementTo[Name]("name"):
  private val nAttribute: Attribute[String] = Attribute("n")

  override def contentType: ContentType = ContentType.Characters

  override def contentParsable: Parsable[Name] = new Parsable[Name]:
    override def parser: Parser[Name] = for
      n: Option[String] <- nAttribute.optional()
      characters: Option[String] <- Text().optional()
      _ <- Effects.check(n.nonEmpty || characters.nonEmpty, "Both 'n' attribute and text are absent.")
      _ <- Effects.check(n.isEmpty  || characters.isEmpty, "Both 'n' attribute and text are present.")
      name: Option[String] = n.orElse(characters)
      languageSpec: Language.Spec <- Language.Spec()
    yield Name(
      name = name.get,
      languageSpec = languageSpec
    )

    override val unparser: Unparser[Name] = Unparser.concat(
      nAttribute.required(_.name),
      Language.Spec(_.languageSpec)
    )
