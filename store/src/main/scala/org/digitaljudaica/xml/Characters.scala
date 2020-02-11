package org.digitaljudaica.xml

import cats.implicits._

object Characters {
  def apply(): Parser[Option[String]] = Context.takeCharacters
}
