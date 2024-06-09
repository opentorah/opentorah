package org.opentorah.collector

import org.opentorah.tei.Pb
import org.opentorah.store.{Context, Path}
import org.opentorah.xml.{A, Attribute, Caching, Xml}
import zio.ZIO

sealed abstract class Page(val pb: Pb):
  def base: String
  def displayName: String

  def reference(context: Context, document: Document, path: Path): Caching.Parser[Xml.Element] =
    for textFacetA: A <- document.textFacetLink(context, path)
    yield pb.addAttributes(textFacetA.setFragment(Pb.pageId(pb.n))(text = displayName))

object Page:

  sealed trait Type derives CanEqual: // all deriveds are objects; using eq
    def name: String
    def apply(pb: Pb): Page

  private object Manuscript extends Type:
    override def name: String = "manuscript"

    private val frontSuffix: String = "-1"
    private val backSuffix: String = "-2"
    require(frontSuffix.length == backSuffix.length)

    private val numberOfDigitsInName: Int = 3

    def base(name: String): String = name.dropRight(frontSuffix.length)

    override def apply(pb: Pb): Page =
      val n = pb.n
      val (base: String, back: Boolean) =
        if n.endsWith(frontSuffix) then (n.dropRight(frontSuffix.length), false) else
          if !n.endsWith(backSuffix) then throw IllegalArgumentException(s"No suffix: $n")
          (n.dropRight(backSuffix.length), true)

      val numberOfDigits = base.takeWhile(_.isDigit).length
      if numberOfDigits < numberOfDigitsInName then // LVIA2 has 4-digit page numbers!
        throw IllegalArgumentException()
      val s: String = base.drop(numberOfDigits)
      if s.nonEmpty && (s != "a") then
        throw IllegalArgumentException(s"Illegal page name: $s [$n]")

      new Manuscript(base, back, pb)

  private final class Manuscript(
    override val base: String,
    back: Boolean,
    pb: Pb
  ) extends Page(pb):
    override def displayName: String = base + (if back then "об" else "")

  private object Book extends Type:
    override def name: String = "book"

    override def apply(pb: Pb): Page =
      val n = pb.n
      if n.dropWhile(_.isDigit).nonEmpty then throw IllegalArgumentException()
      new Book(pb)

  private final class Book(
    pb: Pb
  ) extends Page(pb):
    override def base: String = pb.n
    override def displayName: String = pb.n
  
  val typeAttribute: Attribute.OrDefault[Type] = Attribute.EnumeratedAttribute[Type](
    name = "pageType",
    values = Seq(Manuscript, Book),
    default = Manuscript,
    getName = _.name
  ).orDefault
