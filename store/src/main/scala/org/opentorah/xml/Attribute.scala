package org.opentorah.xml

sealed abstract class Attribute[A](name: String) extends AttributeLike[A]

object Attribute {

  def apply(name: String): StringAttributeLike = new StringAttributeLike {
    override def toString: String = s"attribute $name"

    override def optional: Parser[Option[String]] =
      Context.takeAttribute(name)
  }

  val id: AttributeLike[String] = Attribute("xml:id")

  val allAttributes: Parser[Map[String, String]] =
    Context.takeAllAttributes
}
