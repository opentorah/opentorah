package org.digitaljudaica.xml

object Characters {
  def optional: Parser[Option[String]] = Context.takeCharacters

  def required: Parser[String] = Parser.required("characters", optional)
}
