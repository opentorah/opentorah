package org.opentorah.collector

sealed trait Page {
  def n: String
  def base: String
  def displayName: String
  def facs: Option[String]
  def isPresent: Boolean = facs.isDefined
}

// TODO incorporate into Collection subtype...
object Page {

  sealed trait Type {
    def apply(n: String, facs: Option[String]): Page
  }

  object Manuscript extends Type {
    private val frontSuffix: String = "-1"
    private val backSuffix: String = "-2"
    require(frontSuffix.length == backSuffix.length)

    private val numberOfDigitsInName: Int = 3

    def base(name: String): String = name.dropRight(frontSuffix.length)

    override def apply(n: String, facs: Option[String]): Page = {
      val (base: String, back: Boolean) =
        if (n.endsWith(frontSuffix)) (n.dropRight(frontSuffix.length), false) else {
          if (!n.endsWith(backSuffix)) throw new IllegalArgumentException(s"No suffix: $n")
          (n.dropRight(backSuffix.length), true)
        }

      val numberOfDigits = base.takeWhile(_.isDigit).length
      if (numberOfDigits < numberOfDigitsInName) // LVIA2 has 4-digit page numbers!
        throw new IllegalArgumentException()
      val s: String = base.drop(numberOfDigits)
      if (!s.isEmpty && (s != "a"))
        throw new IllegalArgumentException(s"Illegal page name: $s [$n]")

      new Manuscript(base, back, facs)
    }
  }

  private final class Manuscript(
    override val base: String,
    back: Boolean,
    override val facs: Option[String]
  ) extends Page {
    override def n: String = base + (if (back) Manuscript.backSuffix else Manuscript.frontSuffix)
    override def displayName: String = base + (if (back) "об" else "")
  }

  object Book extends Type {
    override def apply(n: String, facs: Option[String]): Page = {
      if (!n.dropWhile(_.isDigit).isEmpty)
        throw new IllegalArgumentException()
      new Book(n, facs)
    }
  }

  private final class Book(
    override val n: String,
    override val facs: Option[String]
  ) extends Page {
    override val base: String = n
    override def displayName: String = n
  }
}
