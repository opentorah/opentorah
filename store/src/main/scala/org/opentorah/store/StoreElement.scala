package org.opentorah.store

import org.opentorah.metadata.Names
import org.opentorah.xml.{Attribute, Element, Parser, RawXml, ToXml}
import zio.ZIO
import scala.xml.{Elem, Node}

sealed trait StoreElement

object StoreElement extends Element[StoreElement](elementName = "store", parser = for {
  file <- Attribute("file").optional
  result <- if (file.isDefined) StoreElement.fromFileParser(file.get) else StoreElement.inlineParser
} yield result) with ToXml[StoreElement] {

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
    storeType <- Attribute("type").optional
    selectors <- Selector.all
    entities <- EntitiesElement.optional
    title <- Title.parsable.optional
    storeAbstract <- Abstract.parsable.optional
    by <- ByElement.optional
    notes <- Element.allNodes
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

  override def toXml(value: StoreElement): Elem = value match {
    case fromFile: FromFile => toXmlFromFile(fromFile)
    case inline: Inline => toXmlInline(inline)
  }

  private def toXmlFromFile(value: FromFile): Elem =
    <store file={value.file}/>

  private def toXmlInline(value: StoreElement.Inline): Elem = {
    val defaultName: Option[String] = value.names.getDefaultName
    <store
      n={defaultName.orNull}
      from={value.from.orNull}
      type={value.storeType.orNull}
    >
      {if (defaultName.isDefined) Seq.empty else Names.toXml(value.names)}
      {Selector.toXml(value.selectors)}
      {EntitiesElement.toXml(value.entities)}
      {Title.parsable.toXml(value.title)}
      {Abstract.parsable.toXml(value.storeAbstract)}
      {ByElement.toXml(value.by)}
      {value.notes.xml}
    </store>
  }
}
