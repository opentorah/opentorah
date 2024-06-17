package org.opentorah.xml

import org.opentorah.util.{Effects, Files}
import java.net.URL
import zio.ZIO

final class ParserState:

  // Note: using case classes Empty, Characters, Elements and Mixed (in the spirit of "parse, do not validate")
  // leads to much verbosity, since copy method is only available on case classes, which shouldn't be inherited from...
  private final class Current(
    val from: Option[From],
    val name: String,
    val attributes: Attribute.StringValues,
    val contentType: ContentType,
    val nodes: Nodes,
    val characters: Option[String],
    val nextElementNumber: Int
  ):
    def copy(
      attributes: Attribute.StringValues = attributes,
      nodes: Nodes = nodes,
      characters: Option[String] = characters,
      nextElementNumber: Int = nextElementNumber
    ): Current = Current(
      from,
      name,
      attributes,
      contentType,
      nodes,
      characters,
      nextElementNumber
    )

  private var stack: List[Current] = List.empty

  // TODO report error better: effect.tapCause(cause => console.putStrLn(cause.prettyPrint))?
  def addErrorTrace(error: Effects.Error): Effects.Error = Effects.Error(
    message = error.getMessage + "\n" + stack.flatMap(_.from).map(_.url),
    cause = error.getCause
  )

  def checkEmpty(): Unit =
    if stack.nonEmpty then throw IllegalStateException(s"Non-empty $this!")

  private def checkNoLeftovers: Effects.IO[Unit] =
    stack.headOption.fold(Effects.ok)((current: Current) =>
      Effects.check(current.attributes.isEmpty, s"Unparsed attributes: ${current.attributes}") *>
      Effects.check(current.characters.isEmpty, s"Unparsed characters: ${current.characters.get}") *>
      Effects.check(current.nodes.forall(Atom.isWhitespace), s"Unparsed nodes: ${current.nodes}")
    )

  def fromUrl: FromUrl = FromUrl(
    url = stack.flatMap(_.from).head.url.get,
    inline = stack.head.from.isEmpty
  )

  private def modifyCurrent(newCurrent: Current): Unit =
    stack = newCurrent :: stack.tail

  // TODO take namespace into account!
  def optional[A](
    elementsTo: ElementsTo[A]
  ): Parser[Option[A]] =
    val current: Current = stack.head
    for
      _ <- Effects.check(current.contentType.elementsAllowed, s"No element in $current")
      noLeadingWhitespace: Nodes = current.nodes.dropWhile(Atom.isWhitespace)
      result: Option[A] <- noLeadingWhitespace
        .headOption
        .filter(Element.is)
        .map(Element.as)
        .fold(ZIO.none)((xmlElement: Element) => elementsTo
          .elementByName(Element.getName(xmlElement))
          .fold(ZIO.none){ (elementTo: ElementTo[? <: elementsTo.ElementType]) =>
            modifyCurrent(current.copy(
              nodes = noLeadingWhitespace.tail,
              nextElementNumber = current.nextElementNumber + 1
            ))

            for
              // Since I essentially "fix" xml:base attributes above
              // to be relative to the initial document and not to the including one
              // (which is incorrect, but what else can I do?)
              // I need to supply that initial URL for the subUrl calculations...
              base: Option[String] <- Xml.baseAttribute.optional.get(xmlElement)
              fromUrl: Option[URL] = base.map(Files.subUrl(stack.flatMap(_.from).last.url, _))
              result: A <- nested(
                fromUrl.map(From.include),
                elementsTo,
                elementTo,
                Xml.baseAttribute.remove(xmlElement)
              )
            yield Some(result)
          }
        )
    yield result

  def required[A](
    elementsTo: ElementsTo[A],
    from: From
  ): Parser[A] = for
    xmlElement: Element <- from.load
    xmlElementName: String = Element.getName(xmlElement)
    elementTo: ElementTo[? <: elementsTo.ElementType] <- Effects.required(
      ZIO.succeed(elementsTo.elementByName(xmlElementName)),
      s"$elementsTo; found '$xmlElementName' instead"
    )
    _ <- checkNoLeftovers
    result: A <- nested(
      Some(from),
      elementsTo,
      elementTo,
      xmlElement
    )
  yield result

  private def nested[A](
    from: Option[From],
    elementsTo: ElementsTo[A],
    elementTo: ElementTo[? <: elementsTo.ElementType],
    xmlElement: Element
  ): Parser[A] =
    val contentType: ContentType = elementTo.contentType

    // Note: skip comments.
    val nodes: Nodes = Element.getChildren(xmlElement).filterNot(Comment.is)
    val (elements: Elements, characters: Option[String]) = partition(nodes)

    for
      _ <- Effects.check(contentType.elementsAllowed   || elements  .isEmpty, s"Spurious elements: $elements")
      _ <- Effects.check(contentType.charactersAllowed || characters.isEmpty, s"Spurious characters: '${characters.get}'")
      _ =
        val newCurrent: Current = Current(
          from = from,
          name = Element.getName(xmlElement),
          attributes = Attribute.get(xmlElement),
          contentType = contentType,
          nodes = if contentType.elementsAllowed then nodes else Seq.empty,
          characters = if contentType == ContentType.Characters then characters else None,
          nextElementNumber = 0
        )
        stack = newCurrent :: stack
      result: A <- elementsTo.map(elementTo, elementTo.contentParsable())
      _ <- checkNoLeftovers
      _ = stack = stack.tail // pop
    yield result

  // TODO maybe take namespace into account?
  // TODO handle repeated attributes?
  def attribute(attribute: Attribute[?]): Option[String] =
    // TODO Attribute is (and must be!) invariant in T, but here we need to compare
    given CanEqual[Attribute[?], Attribute[String]] = CanEqual.derived

    val current: Current = stack.head
    val (take: Attribute.StringValues, leave: Attribute.StringValues) =
      current.attributes.partition((candidate: Attribute.Value[String]) => attribute == candidate.attribute)
    modifyCurrent(current.copy(attributes = leave))
    take.headOption.flatMap(_.value)

  def attributes: Attribute.StringValues =
    val current: Current = stack.head
    modifyCurrent(current.copy(attributes = Seq.empty))
    current.attributes

  def nodes: Parser[Nodes] =
    val current: Current = stack.head
    for _ <- Effects.check(current.contentType.elementsAllowed, s"No nodes in $current") yield
      modifyCurrent(current.copy(nodes = Seq.empty))
      current.nodes

  def characters: Parser[Option[String]] =
    val current: Current = stack.head

    current.contentType match
      case ContentType.Empty | ContentType.Elements =>
        Effects.fail(s"No characters in $current")

      case ContentType.Characters =>
        ZIO.succeed {
          modifyCurrent(current.copy(characters = None))
          current.characters
        }

      case ContentType.Mixed =>
        val (elements: Elements, characters: Option[String]) = partition(current.nodes)
        for _ <- Effects.check(elements.isEmpty, s"Elements in $current") yield
          modifyCurrent(current.copy(nodes = Seq.empty))
          characters

  private def partition(nodes: Nodes): (Elements, Option[String]) =
    val (elems, nonElems) = nodes.partition(Element.is)
    val characters: String = nonElems.map(Node.toString).mkString.trim
    (elems.map(Element.as), if characters.isEmpty then None else Some(characters))

private[xml] object ParserState:
  def empty: ParserState = new ParserState

  def access[T](f: ParserState => T): Parser[T] = ZIO.service[ParserState].map(f)
  def accessZIO[T](f: ParserState => Parser[T]): Parser[T] = ZIO.service[ParserState].flatMap(f)
