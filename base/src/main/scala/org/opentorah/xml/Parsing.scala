package org.opentorah.xml

import org.opentorah.util.Effects
import java.net.URL
import zio.{Has, Task, ZIO, ZLayer}

/* TODO all the issues with Scala XML push me towards generalizing parsing (and transforms) to work with Dom...
   So far, my attempts to scope this inside final class Parsing(xml: Xml)
   with object Parsing { val scalaXml: Parsing = new Parsing(ScalaXml) }
   even before ScalaXml is changed to xml
   are running into (ZIO 1.0.11):

   zio.FiberFailure: Fiber failed.
   An unchecked error was produced.
   java.lang.Error: Defect in zio.Has: Could not find Parsing::Context inside Map(scalaXml::Context -> org.opentorah.xml.Parsing$Context@77ae5cd0)
     at zio.Has$HasSyntax$.$anonfun$get$extension$2(Has.scala:181)
     at scala.collection.immutable.Map$EmptyMap$.getOrElse(Map.scala:228)
     at zio.Has$HasSyntax$.$anonfun$get$extension$1(Has.scala:181)
     at scala.collection.immutable.Map$Map1.getOrElse(Map.scala:250)
     at zio.Has$HasSyntax$.get$extension(Has.scala:179)
 */

type Parser[+A] = ZIO[Has[Parsing], Effects.Error, A]

final class Parsing:
  private var stack: List[Parsing.Current] = List.empty

object Parsing:

  def unsafeRun[A](parser: Parser[A]): A = Effects.unsafeRun(toTask(parser))

  // TODO report error better: effect.tapCause(cause => console.putStrLn(cause.prettyPrint))
  // see nested()
  def toTask[A](parser: Parser[A]): Task[A] = Effects.error2throwable(
    for
      result: A <- parser
      _ <- checkEmpty
    yield result
  ).provideLayer(ZLayer.succeed(new Parsing))

  // Note: using case classes Empty, Characters, Elements and Mixed (in the spirit of "parse, do not validate")
  // leads to much verbosity, since copy method is only available on case classes, which shouldn't be inherited from...
  private final case class Current(
    from: Option[From],
    name: String,
    attributes: Attribute.StringValues,
    contentType: ContentType,
    nodes: ScalaXml.Nodes = Seq.empty,
    characters: Option[String] = None,
    nextElementNumber: Int = 0
  )

  // TODO look into simplifying:
  private[xml] def nested[A](
    from: Option[From],
    nextElement: ScalaXml.Element,
    contentType: ContentType,
    parser: Parser[A]
  ): Parser[A] = for 
    newCurrent: Current <- open(
      from: Option[From],
      nextElement,
      contentType
    )
    result: A <- access((parsing: Parsing) => parsing.stack = newCurrent :: parsing.stack).bracket[Has[Parsing], Effects.Error, A](
      release = (_: Unit) => ZIO.access[Has[Parsing]]((parsingHas: Has[Parsing]) =>
        val parsing = parsingHas.get
        parsing.stack = parsing.stack.tail
      ),
      use = (_: Unit) => addErrorTrace(
        for
          result: A <- parser
          _ <- checkNoLeftovers 
        yield result
      )
    )
  yield result

  private def open(
    from: Option[From],
    element: ScalaXml.Element,
    contentType: ContentType
  ): zio.IO[Effects.Error, Current] =
    val name: String = ScalaXml.getName(element)
    val attributes: Attribute.StringValues = ScalaXml.getAttributes(element)
    val nodes: ScalaXml.Nodes = ScalaXml.getChildren(element)
    val (elements: Seq[ScalaXml.Element], characters: Option[String]) = partition(nodes)

    contentType match
      case ContentType.Empty =>
        Effects.check(elements.isEmpty, s"Spurious elements: $elements") *>
        Effects.check(characters.isEmpty, s"Spurious characters: '${characters.get}'") *>
        ZIO.succeed(Current(
          from = from,
          name = name,
          attributes = attributes,
          contentType = ContentType.Empty
        ))

      case ContentType.Characters =>
        Effects.check(elements.isEmpty, s"Spurious elements: $elements") *>
        ZIO.succeed(Current(
          from = from,
          name = name,
          attributes = attributes,
          contentType = ContentType.Characters,
          characters = characters
        ))

      case ContentType.Elements =>
        Effects.check(characters.isEmpty, s"Spurious characters: '${characters.get}'") *>
        ZIO.succeed(Current(
          from = from,
          name = name,
          attributes = attributes,
          contentType = ContentType.Elements,
          nodes = nodes
        ))

      case ContentType.Mixed =>
        ZIO.succeed(Current(
          from = from,
          name = name,
          attributes = attributes,
          contentType = ContentType.Mixed,
          nodes = nodes
        ))

  private val checkEmpty: Parser[Unit] = access((parsing: Parsing) =>
    if parsing.stack.nonEmpty then throw IllegalStateException(s"Non-empty $parsing!")
  )

  private[xml] val checkNoLeftovers: Parser[Unit] = access((parsing: Parsing) =>
    parsing.stack.headOption.fold(Effects.ok)((current: Current) =>
      Effects.check(current.attributes.isEmpty, s"Unparsed attributes: ${current.attributes}") *>
      Effects.check(current.characters.isEmpty, s"Unparsed characters: ${current.characters.get}") *>
      Effects.check(current.nodes.forall(ScalaXml.isWhitespace), s"Unparsed nodes: ${current.nodes}")
    )
  )

  private[xml] val currentBaseUrl: Parser[Option[URL]] = access((parsing: Parsing) =>
    currentBaseUrl(parsing)
  )

  private[xml] val currentFromUrl: Parser[FromUrl] = access((parsing: Parsing) => FromUrl(
    url = currentBaseUrl(parsing).get,
    inline = parsing.stack.head.from.isEmpty
  ))

  private def currentBaseUrl(parsing: Parsing): Option[URL] = parsing.stack.flatMap(_.from).head.url

  private[xml] def takeAttribute(attribute: Attribute[?]): Parser[Option[String]] = modifyCurrent((current: Current) =>
    // TODO maybe take namespace into account?
    val (take: Attribute.StringValues, leave: Attribute.StringValues) =
      current.attributes.partition((candidate: Attribute.Value[String]) => attribute == candidate.attribute)

    // TODO handle repeated attributes?
    val result: Option[String] = take.headOption.flatMap(_.value)

    ZIO.succeed((result, current.copy(attributes = leave)))
  )

  private[xml] val allAttributes: Parser[Attribute.StringValues] = modifyCurrent((current: Current) =>
    ZIO.succeed((current.attributes, current.copy(attributes = Seq.empty)))
  )

  private[xml] def nextElement(p: ScalaXml.Predicate): Parser[Option[ScalaXml.Element]] = modifyCurrent((current: Current) =>
    current.contentType match
      case ContentType.Empty | ContentType.Characters =>
        Effects.fail(s"No element in $current")

      case ContentType.Elements | ContentType.Mixed =>
        val noLeadingWhitespace: Seq[ScalaXml.Node] = current.nodes.dropWhile(ScalaXml.isWhitespace)
        val result: Option[ScalaXml.Element] =
          noLeadingWhitespace.headOption.filter(ScalaXml.isElement).map(ScalaXml.asElement).filter(p)
        val newCurrent: Current =
          if result.isEmpty then current
          else current.copy(nodes = noLeadingWhitespace.tail, nextElementNumber = current.nextElementNumber+1)

        ZIO.succeed((result, newCurrent))
  )

  private[xml] val takeCharacters: Parser[Option[String]] = modifyCurrent((current: Current) => current.contentType match
    case ContentType.Empty | ContentType.Elements =>
      Effects.fail(s"No characters in $current")

    case ContentType.Characters =>
      ZIO.succeed((current.characters, current.copy(characters = None)))

    case ContentType.Mixed =>
      val (elements: Seq[ScalaXml.Element], characters: Option[String]) = partition(current.nodes)
      Effects.check(elements.isEmpty, s"Elements in $current") *>
      ZIO.succeed((characters, current.copy(nodes = Seq.empty)))
  )

  private[xml] val allNodes: Parser[ScalaXml.Nodes] = modifyCurrent((current: Current) => current.contentType match
    case ContentType.Empty | ContentType.Characters =>
      Effects.fail(s"No nodes in $current")

    case ContentType.Elements | ContentType.Mixed =>
      ZIO.succeed((current.nodes, current.copy(nodes = Seq.empty)))
  )

  private def partition(nodes: ScalaXml.Nodes): (Seq[ScalaXml.Element], Option[String]) =
    val (elems, nonElems) = nodes.partition(ScalaXml.isElement)
    val characters: String = nonElems.map(ScalaXml.toString).mkString.trim
    (elems.map(ScalaXml.asElement), if characters.isEmpty then None else Some(characters))

  private def addErrorTrace[A](parser: Parser[A]): Parser[A] = parser.flatMapError((error: Effects.Error) =>
    ZIO.access[Has[Parsing]]((parsingHas: Has[Parsing]) =>
      new Effects.Error(error.getMessage + "\n" + parsingHas.get.stack.headOption.map(_.toString).getOrElse(""))
    )
  )

  private def access[T](f: Parsing => T): Parser[T] = ZIO.access[Has[Parsing]]((parsingHas: Has[Parsing]) =>
    f(parsingHas.get)
  )

  private def modifyCurrent[A](f: Current => zio.IO[Effects.Error, (A, Current)]): Parser[A] =
    ZIO.accessM[Has[Parsing]]((parsingHas: Has[Parsing]) =>
      val parsing: Parsing = parsingHas.get
      for
        out: (A, Current) <- f(parsing.stack.head)
        (result: A, newCurrent: Current) = out
        _ <- ZIO.succeed(parsing.stack = newCurrent :: parsing.stack.tail)
      yield result
    )
