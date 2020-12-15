package org.opentorah.collectorng

import org.opentorah.metadata.Names
import org.opentorah.tei.Title
import org.opentorah.xml.{Antiparser, Attribute, Element, FromUrl, Parser}

// TODO ByDocument
final class CollectionBy(
  override val fromUrl: FromUrl,
  override val selector: Selector,
  val directory: String,
  val parts: Seq[CollectionBy.Part]
) extends By {
  override protected def resolveStore(url: Seq[String]): Option[SiteFile] = None // TODO
}

object CollectionBy extends Element[CollectionBy]("by") {
  private val selectorAttribute: Attribute[String] = Attribute("selector")
  private val directoryAttribute: Attribute[String] = Attribute("directory")

  override def parser: Parser[CollectionBy] = for {
    fromUrl <- currentFromUrl
    selector <- selectorAttribute.required
    directory <- directoryAttribute.required
    parts <- Part.all
  } yield new CollectionBy(
    fromUrl,
    Selector.byName(selector),
    directory,
    parts
  )

  override def antiparser: Antiparser[CollectionBy] = Antiparser.concat(
    selectorAttribute.toXml.compose(_.selector.name),
    directoryAttribute.toXml.compose(_.directory)
  )

  final class Part(
    val names: Names,
    val from: String,
    val title: Title.Value
  )

  object Part extends Element[Part]("store") {
    private val fromAttribute: Attribute[String] = Attribute("from")

    override def parser: Parser[Part] = for {
      names <- Names.withDefaultNameParser
      from <- fromAttribute.required
      title <- Title.parsable.required
    } yield new Part(
      names,
      from,
      title
    )

    override def antiparser: Antiparser[Part] = Antiparser.concat(
      Names.antiparser.compose(_.names),
      fromAttribute.toXml.compose(_.from),
      Title.parsable.toXml.compose(_.title)
    )
  }
}
