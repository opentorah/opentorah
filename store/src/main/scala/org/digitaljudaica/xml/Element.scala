package org.digitaljudaica.xml

import cats.implicits._

object Element extends ElementMethods(charactersAllowed = false) {

  val name: Parser[String] = Context.getName

  object nextNested {
    val name: Parser[Option[String]] = Context.getNextNestedElementName

    def nameIs(expected: String): Parser[Boolean] = name.map(_.contains(expected))

    def nameIsNot(expected: String): Parser[Boolean] = nameIs(expected).map(result => !result)

    // TODO simplify
    def nameIsNot(expected: Option[String]): Parser[Boolean] = for {
      nextNestedElementName <- name
    } yield nextNestedElementName.isEmpty || expected.fold(false)(expected => !nextNestedElementName.contains(expected))
  }

  object withCharacters extends ElementMethods(charactersAllowed = true)
}
