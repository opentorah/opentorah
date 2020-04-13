package org.opentorah.collector

import org.opentorah.tei.Pb

sealed abstract class Page(val pb: Pb) {
  def base: String
  def displayName: String
}

object Page {

  sealed trait Type {
    def apply(pb: Pb): Page
  }

  object Manuscript extends Type {
    private val frontSuffix: String = "-1"
    private val backSuffix: String = "-2"
    require(frontSuffix.length == backSuffix.length)

    private val numberOfDigitsInName: Int = 3

    def base(name: String): String = name.dropRight(frontSuffix.length)

    override def apply(pb: Pb): Page = {
      val n = pb.n
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

      new Manuscript(base, back, pb)
    }
  }

  private final class Manuscript(
    override val base: String,
    back: Boolean,
    pb: Pb
  ) extends Page(pb) {
    override def displayName: String = base + (if (back) "об" else "")
  }

  object Book extends Type {
    override def apply(pb: Pb): Page = {
      val n = pb.n
      if (!n.dropWhile(_.isDigit).isEmpty)
        throw new IllegalArgumentException()
      new Book(pb)
    }
  }

  private final class Book(
    pb: Pb
  ) extends Page(pb) {
    override def base: String = pb.n
    override def displayName: String = pb.n
  }

  def pageId(n: String): String = s"p$n"

  def pageRendition(isMissing: Boolean): String = if (isMissing) "missing-page" else "page"
}
