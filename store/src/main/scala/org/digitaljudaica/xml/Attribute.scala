package org.digitaljudaica.xml

import cats.implicits._

object Attribute {

  object optional {
    def apply(name: String): Parser[Option[String]] = Context.takeAttribute(name)

    def boolean(name: String): Parser[Option[Boolean]] = for {
      resultO <- Attribute.optional(name)
      result = resultO.map(value => value == "true" || value == "yes")
    } yield result

    def int(name: String, nonPositiveAllowed: Boolean = false): Parser[Option[Int]] = for {
      resultO <- Attribute.optional(name)
      result <- Parser.toParser(resultO.map(_.toInt))
      _ <- Parser.check(result.isEmpty || nonPositiveAllowed || (result.get > 0),
        s"Non-positive integer: ${result.get}")
    } yield result
  }

  object required {
    def apply(name: String): Parser[String] =
      Parser.required(s"attribute '$name'", optional(name))

    def boolean(name: String): Parser[Boolean] =
      Parser.required(s"boolean attribute '$name'", optional.boolean(name))

    def int(name: String): Parser[Int] =
      Parser.required(s"integer attribute '$name'", optional.int(name))
  }
}
