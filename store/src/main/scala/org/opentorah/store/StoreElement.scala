package org.opentorah.store

import java.net.URL
import org.opentorah.metadata.Names
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Element, From, Parser, RawXml}
import zio.ZIO
import scala.xml.Node

sealed trait StoreElement

object StoreElement extends Element[StoreElement](elementName = "store", parser = for {
  file <- Attribute("file").optional
  result <- if (file.isDefined) StoreElement.fromFileParser(file.get) else StoreElement.inlineParser
} yield result) {

  final case class FromFile(
    file: String
  ) extends StoreElement

  private def fromFileParser(file: String): Parser[FromFile] =
    ZIO.succeed(FromFile(file))

  object Title extends RawXml("title")

  object Abstract extends RawXml("abstract")

  final class Notes(val xml: Seq[Node])

  final case class Inline(
    names: Names,
    // TODO this is here only to allow reuse of StoreElement for Collection's parts;
    // it should be removed or generalized - but I am not ready to make StoreElement parsing extendable yet ;)
    from: Option[String],
    selectors: Seq[Selector],
    by: Option[ByElement],
    entities: Option[EntitiesElement],
    title: Option[Title.Value],
    storeAbstract: Option[Abstract.Value],
    notes: Notes,
    storeType: Option[String]
  ) extends StoreElement

  private val inlineParser: Parser[Inline] = for {
    names <- Names.withDefaultNameParser
    from <- Attribute("from").optional
    selectors <- Selector.all
    by <- ByElement.optional
    entities <- EntitiesElement.optional
    title <- Title.parsable.optional
    storeAbstract <- Abstract.parsable.optional
    notes <- Element.allNodes
    storeType <- Attribute("type").optional
  } yield StoreElement.Inline(
    names,
    from,
    selectors,
    by,
    entities,
    title,
    storeAbstract,
    new Notes(notes),
    storeType
  )

  @scala.annotation.tailrec
  def resolve(url: URL, element: StoreElement): (URL, StoreElement.Inline) = element match {
    case result: StoreElement.Inline =>
      (url, result)

    case FromFile(file) =>
      val nextUrl = Files.fileInDirectory(url, file)
      val next: StoreElement = Parser.parseDo(StoreElement.parse(From.url(nextUrl)))
      resolve(nextUrl, next)
  }

  def read(url: URL): StoreElement =
    Parser.parseDo(StoreElement.parse(From.url(url)))
}
