package org.digitaljudaica.archive.collector

sealed trait Page {
  def name: String
  def base: String
  def displayName: String
  def isPresent: Boolean
}

object Page {

  sealed trait Type {
    def apply(name: String, isPresent: Boolean): Page
  }

  object Manuscript extends Type {
    private val frontSuffix: String = "-1"
    private val backSuffix: String = "-2"
    require(frontSuffix.length == backSuffix.length)

    private val numberOfDigitsInName: Int = 3

    def base(name: String): String = name.dropRight(frontSuffix.length)

    override def apply(name: String, isPresent: Boolean): Page = {
      val (base: String, back: Boolean) =
        if (name.endsWith(frontSuffix)) (name.dropRight(frontSuffix.length), false) else {
          if (!name.endsWith(backSuffix)) throw new IllegalArgumentException(s"No suffix: $name")
          (name.dropRight(backSuffix.length), true)
        }

      if (base.takeWhile(_.isDigit).length != numberOfDigitsInName)
        throw new IllegalArgumentException()
      val s: String = base.drop(numberOfDigitsInName)
      if (!s.isEmpty && (s != "a"))
        throw new IllegalArgumentException(s"Illegal page name: $s [$name]")

      new Manuscript(base, back, isPresent)
    }
  }

  private final class Manuscript(
    override val base: String,
    back: Boolean,
    override val isPresent: Boolean
  ) extends Page {
    override def name: String = base + (if (back) Manuscript.backSuffix else Manuscript.frontSuffix)
    override def displayName: String = base + (if (back) "об" else "")
  }

  object Book extends Type {
    override def apply(name: String, isPresent: Boolean): Page = {
      if (!name.dropWhile(_.isDigit).isEmpty)
        throw new IllegalArgumentException()
      new Book(name, isPresent)
    }
  }

  private final class Book(
    override val name: String,
    override val isPresent: Boolean
  ) extends Page {
    override val base: String = name
    override def displayName: String = name
  }
}
