package org.digitaljudaica.xml

import cats.implicits._

object Attribute {

  object optional {
    def apply(name: String): Parser[Option[String]] = Context.takeAttribute(name)

    def boolean(name: String): Parser[Option[Boolean]] = for {
      resultO <- Attribute.optional(name)
      result = resultO.map(value => value == "true" || value == "yes")
    } yield result

    // TODO rename positiveInt
    def int(name: String): Parser[Option[Int]] = for {
      resultO <- Attribute.optional(name)
      result <-
        try { Parser.pure(resultO.map(_.toInt)) }
        catch { case e: NumberFormatException => Parser.error(e.getMessage) }
      _ <- Parser.check(result.isEmpty || result.get > 0, s"Non-positive integer: ${result.get}")
    } yield result
  }

  object required {
    def apply(name: String): Parser[String] =
      Parser.required(s"attribute '$name'", Attribute.optional(name))

    def boolean(name: String): Parser[Boolean] =
      Parser.required(s"boolean attribute '$name'", Attribute.optional.boolean(name))

    def int(name: String): Parser[Int] =
      Parser.required(s"integer attribute '$name'", Attribute.optional.int(name))
  }
}
