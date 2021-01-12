package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.tei.Title
import org.opentorah.xml.{Unparser, Attribute, Element, Parsable, Parser}

final class CollectionPart(
  val names: Names,
  val from: String,
  val title: Title.Value
) {
  def take(documents: Seq[Document]): CollectionPart.Part = {
    if (documents.isEmpty) throw new IllegalArgumentException("No documents for Part!")
    if (documents.head.name != from) throw new IllegalArgumentException("Incorrect 'from' document")
    new CollectionPart.Part(Some(title), documents)
  }
}

// TODO rename the element "part" when new generation starts to rule
object CollectionPart extends Element[CollectionPart]("store") {

  final class Part(
    val title: Option[Title.Value],
    val documents: Seq[Document]
  )

  def getParts(
    parts: Seq[CollectionPart],
    documents: Seq[Document]
  ): Seq[Part] =
    if (parts.isEmpty) Seq(new Part(None, documents))
    else splitParts(Seq.empty, parts, documents)

  @scala.annotation.tailrec
  private def splitParts(
    result: Seq[Part],
    parts: Seq[CollectionPart],
    documents: Seq[Document]
  ): Seq[Part] = parts match {
    case p1 :: p2 :: ds =>
      val (partDocuments: Seq[Document], tail: Seq[Document]) = documents.span(_.name != p2.from)
      splitParts(result :+ p1.take(partDocuments), p2 :: ds, tail)

    case p1 :: Nil =>
      result :+ p1.take(documents)

    case Nil =>
      if (documents.nonEmpty)
        throw new IllegalArgumentException("Documents left over: " + documents.mkString(", ") + ".")
      result
  }

  private val fromAttribute = Attribute("from").required

  override def contentParsable: Parsable[CollectionPart] = new Parsable[CollectionPart] {
    override def parser: Parser[CollectionPart] = for {
      names <- Names.withDefaultNameParsable()
      from <- fromAttribute()
      title <- Title.element.required()
    } yield new CollectionPart(
      names,
      from,
      title
    )

    override def unparser: Unparser[CollectionPart] = Unparser.concat(
      Names.withDefaultNameParsable(_.names),
      fromAttribute(_.from),
      Title.element.required(_.title)
    )
  }
}
