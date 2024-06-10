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
    val contentType: Element.ContentType,
    val nodes: Xml.Nodes,
    val characters: Option[String],
    val nextElementNumber: Int
  ):
    def copy(
      attributes: Attribute.StringValues = attributes,
      nodes: Xml.Nodes = nodes,
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
  private def addErrorTrace(error: Effects.Error): Effects.Error = Effects.Error(
    message = error.getMessage + "\n" + stack.flatMap(_.from).map(_.url),
    cause = error.getCause
  )

  private def modifyCurrent(newCurrent: Current): Unit =
    stack = newCurrent :: stack.tail

  private def checkEmpty(): Unit =
    if stack.nonEmpty then throw IllegalStateException(s"Non-empty $this!")

  private def checkNoLeftovers: Effects.IO[Unit] =
    stack.headOption.fold(Effects.ok)((current: Current) =>
      Effects.check(current.attributes.isEmpty, s"Unparsed attributes: ${current.attributes}") *>
      Effects.check(current.characters.isEmpty, s"Unparsed characters: ${current.characters.get}") *>
      Effects.check(current.nodes.forall(Xml.isWhitespace), s"Unparsed nodes: ${current.nodes}")
    )

  private def currentFromUrl: Element.FromUrl = Element.FromUrl(
    url = currentBaseUrl.get,
    inline = stack.head.from.isEmpty
  )

  private def currentBaseUrl: Option[URL] =
    stack.flatMap(_.from).head.url

  private def initialBaseUrl: Option[URL] =
    stack.flatMap(_.from).last.url

  private def push(
    from: Option[From],
    nextElement: ParserState.NextElement[?]
  ): Effects.IO[Unit] =
    val contentType: Element.ContentType = nextElement.elementAndParser.element.contentType
    val xmlElement: Xml.Element = nextElement.xmlElement
    // Note: skip comments.
    val nodes: Xml.Nodes = Xml.getChildren(xmlElement).filterNot(Xml.isComment)
    val (elements: Seq[Xml.Element], characters: Option[String]) = partition(nodes)

    Effects.check(contentType.elementsAllowed  || elements.isEmpty  , s"Spurious elements: $elements") *>
    Effects.check(contentType.charactersAlowed || characters.isEmpty, s"Spurious characters: '${characters.get}'") *>
    ZIO.succeed {
      val newCurrent: Current = Current(
        from = from,
        name = Xml.getName(xmlElement),
        attributes = Attribute.get(xmlElement),
        contentType = contentType,
        nodes = if contentType.elementsAllowed then nodes else Seq.empty,
        characters = if contentType == Element.ContentType.Characters then characters else None,
        nextElementNumber = 0
      )
      stack = newCurrent :: stack
    }

  private def pop(): Unit =
    stack = stack.tail

  // TODO maybe take namespace into account?
  // TODO handle repeated attributes?
  private def takeAttribute(attribute: Attribute[?]): Option[String] =
    // TODO Attribute is (and must be!) invariant in T, but here we need to compare
    given CanEqual[Attribute[?], Attribute[String]] = CanEqual.derived

    val current: Current = stack.head
    val (take: Attribute.StringValues, leave: Attribute.StringValues) =
      current.attributes.partition((candidate: Attribute.Value[String]) => attribute == candidate.attribute)
    modifyCurrent(current.copy(attributes = leave))
    take.headOption.flatMap(_.value)

  private def allAttributes: Attribute.StringValues =
    val current: Current = stack.head
    modifyCurrent(current.copy(attributes = Seq.empty))
    current.attributes

  private def nextElement[A](elements: Elements[A]): Parser[Option[ParserState.NextElement[A]]] =
    val current: Current = stack.head
    Effects.check(current.contentType.elementsAllowed, s"No element in $current") *>
    ZIO.succeed {
      val noLeadingWhitespace: Xml.Nodes = current.nodes.dropWhile(Xml.isWhitespace)
      val headElement: Option[Xml.Element] =
        noLeadingWhitespace.headOption.filter(Xml.isElement).map(Xml.asElement)
      headElement.map(Xml.getName).flatMap(elements.elementAndParser).map { (result: Element.AndParser[A]) =>
        modifyCurrent(current.copy(nodes = noLeadingWhitespace.tail, nextElementNumber = current.nextElementNumber + 1))
        ParserState.NextElement(headElement.get, result)
      }
    }

  private def takeCharacters: Parser[Option[String]] =
    val current: Current = stack.head

    current.contentType match
      case Element.ContentType.Empty | Element.ContentType.Elements =>
        Effects.fail(s"No characters in $current")

      case Element.ContentType.Characters =>
        ZIO.succeed {
          modifyCurrent(current.copy(characters = None))
          current.characters
        }

      case Element.ContentType.Mixed =>
        val (elements: Seq[Xml.Element], characters: Option[String]) = partition(current.nodes)
        Effects.check(elements.isEmpty, s"Elements in $current") *>
        ZIO.succeed {
          modifyCurrent(current.copy(nodes = Seq.empty))
          characters
        }

  private def allNodes: Parser[Xml.Nodes] =
    val current: Current = stack.head
    Effects.check(current.contentType.elementsAllowed, s"No nodes in $current") *>
    ZIO.succeed {
      modifyCurrent(current.copy(nodes = Seq.empty))
      current.nodes
    }

  private def partition(nodes: Xml.Nodes): (Seq[Xml.Element], Option[String]) =
    val (elems, nonElems) = nodes.partition(Xml.isElement)
    val characters: String = nonElems.map(Xml.toString).mkString.trim
    (elems.map(Xml.asElement), if characters.isEmpty then None else Some(characters))

private[xml] object ParserState:
  private final case class NextElement[A](xmlElement: Xml.Element, elementAndParser: Element.AndParser[A])

  def empty: ParserState = new ParserState

  private def access[T](f: ParserState => T): Parser[T] = ZIO.service[ParserState].map(f)
  private def accessZIO[T](f: ParserState => Parser[T]): Parser[T] = ZIO.service[ParserState].flatMap(f)

  def fromUrl: Parser[Element.FromUrl] = access(_.currentFromUrl)
  def currentBaseUrl: Parser[Option[URL]] = access(_.currentBaseUrl)
  def allNodes: Parser[Xml.Nodes] = accessZIO(_.allNodes)
  def allAttributes: Parser[Attribute.StringValues] = access(_.allAttributes)
  def takeAttribute(attribute: Attribute[?]): Parser[Option[String]] = access(_.takeAttribute(attribute))
  def takeCharacters: Parser[Option[String]] = accessZIO(_.takeCharacters)
  def checkEmpty: Parser[Unit] = access(_.checkEmpty())

  def addErrorTrace(error: Effects.Error): zio.URIO[ParserState, Effects.Error] =
    ZIO.service[ParserState].map(_.addErrorTrace(error))

  // Since I essentially "fix" xml:base attributes above
  // to be relative to the initial document and not to the including one
  // (which is incorrect, but what else can I do?)
  // I need to supply that initial URL for the subUrl calculations...
  private def parentBase: Parser[Option[URL]] = access(_.initialBaseUrl)

  def optional[A](elements: Elements[A]): Parser[Option[A]] = for
    // TODO take namespace into account!
    nextElementOpt: Option[NextElement[A]] <- accessZIO(_.nextElement(elements))
    result: Option[A] <- nextElementOpt.fold(ZIO.none) { (nextElement: NextElement[A]) =>
      for
        parentBase: Option[URL] <- parentBase
        base: Option[String] <- Xml.baseAttribute.optional.get(nextElement.xmlElement)
        fromUrl: Option[URL] = base.map(Files.subUrl(parentBase, _))
        result: A <- nested(
          from = fromUrl.map(From.include),
          nextElement = fromUrl match
            case None => nextElement
            case _ => nextElement.copy(xmlElement = Xml.baseAttribute.remove(nextElement.xmlElement))
        )
      yield Some(result)
    }
  yield result

  final def required[A](elements: Elements[A], from: From): Parser[A] = for
    xmlElement: Xml.Element <- from.load
    xmlElementName: String = Xml.getName(xmlElement)
    elementAndParserOpt: Option[Element.AndParser[A]] = elements.elementAndParser(xmlElementName)
    _ <- Effects.check(elementAndParserOpt.isDefined, s"$elements required, but '$xmlElementName' found")
    elementAndParser: Element.AndParser[A] = elementAndParserOpt.get
    _ <- accessZIO(_.checkNoLeftovers)
    result: A <- nested(
      from = Some(from),
      nextElement = NextElement(xmlElement, elementAndParser)
    )
  yield result

  private def nested[A](
    from: Option[From],
    nextElement: NextElement[A]
  ): Parser[A] = for
    _ <- accessZIO(_.push(from, nextElement))
    result: A <- nextElement.elementAndParser.parser
    _ <- accessZIO(_.checkNoLeftovers)
    _ <- access(_.pop())
  yield result


