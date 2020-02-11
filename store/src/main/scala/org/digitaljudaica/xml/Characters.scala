package org.digitaljudaica.xml

object Characters {
  def apply(): Parser[Option[String]] = Context.takeCharacters
}
