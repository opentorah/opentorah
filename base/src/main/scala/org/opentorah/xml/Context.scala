package org.opentorah.xml

import org.opentorah.util.Effects
import java.net.URL
import zio.{Has, ZIO}

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

final class Context {
  private var stack: List[Context.Current] = List.empty
}

private[xml] object Context {

  // Note: using case classes Empty, Characters, Elements and Mixed (in the spirit of "parse, do not validate")
  // leads to much verbosity, since copy method is only available on case classes, which shouldn't be inherited from...
  private final case class Current(
    from: Option[From],
    name: String,
    attributes: Seq[Attribute.Value[String]],
    contentType: ContentType,
    nodes: ScalaXml.Nodes = Seq.empty,
    characters: Option[String] = None,
    nextElementNumber: Int = 0
  )

  // TODO look into simplifying:
  def nested[A](
    from: Option[From],
    nextElement: ScalaXml.Element,
    contentType: ContentType,
    parser: Parser[A]
  ): Parser[A] = for  {
    newCurrent <- open(
      from: Option[From],
      nextElement,
      contentType
    )
    result <- access { (context: Context) => context.stack = newCurrent :: context.stack }.bracket[Has[Context], Effects.Error, A](
      release = (_: Unit) => ZIO.access[Has[Context]] { (contextHas: Has[Context]) =>
        val context = contextHas.get
        context.stack = context.stack.tail
      },
      use = (_: Unit) => addErrorTrace(for { result <- parser; _ <- checkNoLeftovers } yield result)
    )
  }  yield result

  private def open(
    from: Option[From],
    element: ScalaXml.Element,
    contentType: ContentType
  ): zio.IO[Effects.Error, Current] = {
    val name: String = ScalaXml.getName(element)
    val attributes: Seq[Attribute.Value[String]] = ScalaXml.getAttributes(element)
    val nodes: ScalaXml.Nodes = ScalaXml.getChildren(element)
    val (elements: Seq[ScalaXml.Element], characters: Option[String]) = partition(nodes)

    contentType match {
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
    }
  }

  val checkEmpty: Parser[Unit] = access { (context: Context) =>
    if (context.stack.nonEmpty) throw new IllegalStateException(s"Non-empty context $context!")
  }

  val checkNoLeftovers: Parser[Unit] = access { (context: Context) =>
    context.stack.headOption.fold(Effects.ok) { (current: Current) =>
      Effects.check(current.attributes.isEmpty, s"Unparsed attributes: ${current.attributes}") *>
      Effects.check(current.characters.isEmpty, s"Unparsed characters: ${current.characters.get}") *>
      Effects.check(current.nodes.forall(ScalaXml.isWhitespace), s"Unparsed nodes: ${current.nodes}")
    }
  }

  val currentBaseUrl: Parser[Option[URL]] = access { (context: Context) =>
    currentBaseUrl(context)
  }

  val currentFromUrl: Parser[FromUrl] = access { (context: Context) => new FromUrl(
    url = currentBaseUrl(context).get,
    inline = context.stack.head.from.isEmpty
  )}

  private def currentBaseUrl(context: Context): Option[URL] = context.stack.flatMap(_.from).head.url

  def takeAttribute(attribute: Attribute[_]): Parser[Option[String]] = modifyCurrent { (current: Current) =>
    // TODO maybe take namespace into account?
    val (take: Seq[Attribute.Value[String]], leave: Seq[Attribute.Value[String]]) =
      current.attributes.partition((candidate: Attribute.Value[String]) => attribute == candidate.attribute)

    // TODO handle repeated attributes?
    val result: Option[String] = take.headOption.flatMap(_.value)

    ZIO.succeed((result, current.copy(attributes = leave)))
  }

  val allAttributes: Parser[Seq[Attribute.Value[String]]] = modifyCurrent { (current: Current) =>
    ZIO.succeed((current.attributes, current.copy(attributes = Seq.empty)))
  }

  def nextElement(p: ScalaXml.Predicate): Parser[Option[ScalaXml.Element]] = modifyCurrent { (current: Current) =>
    current.contentType match {
      case ContentType.Empty | ContentType.Characters =>
        ZIO.fail(s"No element in $current")

      case ContentType.Elements | ContentType.Mixed =>
        val noLeadingWhitespace: Seq[ScalaXml.Node] = current.nodes.dropWhile(ScalaXml.isWhitespace)
        val result: Option[ScalaXml.Element] =
          noLeadingWhitespace.headOption.filter(ScalaXml.isElement).map(ScalaXml.asElement).filter(p)
        val newCurrent: Current =
          if (result.isEmpty) current
          else current.copy(nodes = noLeadingWhitespace.tail, nextElementNumber = current.nextElementNumber+1)

        ZIO.succeed((result, newCurrent))
    }
  }

  val takeCharacters: Parser[Option[String]] = modifyCurrent { (current: Current) => current.contentType match {
    case ContentType.Empty | ContentType.Elements =>
      ZIO.fail(s"No characters in $current")

    case ContentType.Characters =>
      ZIO.succeed((current.characters, current.copy(characters = None)))

    case ContentType.Mixed =>
      val (elements: Seq[ScalaXml.Element], characters: Option[String]) = partition(current.nodes)
      Effects.check(elements.isEmpty, s"Elements in $current") *>
      ZIO.succeed((characters, current.copy(nodes = Seq.empty)))
  }}

  val allNodes: Parser[ScalaXml.Nodes] = modifyCurrent { (current: Current) => current.contentType match {
    case ContentType.Empty | ContentType.Characters =>
      ZIO.fail(s"No nodes in $current")

    case ContentType.Elements | ContentType.Mixed =>
      ZIO.succeed((current.nodes, current.copy(nodes = Seq.empty)))
  }}

  private def partition(nodes: ScalaXml.Nodes): (Seq[ScalaXml.Element], Option[String]) = {
    val (elems: Seq[ScalaXml.Element], nonElems: Seq[ScalaXml.Element]) = nodes.partition(ScalaXml.isElement)
    val characters: String = nonElems.map(ScalaXml.toString).mkString.trim
    (elems.map(ScalaXml.asElement), if (characters.isEmpty) None else Some(characters))
  }

  private def addErrorTrace[A](parser: Parser[A]): Parser[A] = parser.flatMapError((error: Effects.Error) =>
    ZIO.access[Has[Context]]((contextHas: Has[Context]) =>
      error + "\n" + contextHas.get.stack.headOption.map(_.toString).getOrElse("")
    )
  )

  private def access[T](f: Context => T): Parser[T] = ZIO.access[Has[Context]]((contextHas: Has[Context]) =>
    f(contextHas.get)
  )

  private def modifyCurrent[A](f: Current => zio.IO[Effects.Error, (A, Current)]): Parser[A] =
    ZIO.accessM[Has[Context]] { (contextHas: Has[Context]) =>
      val context: Context = contextHas.get
      for {
        out <- f(context.stack.head)
        (result, newCurrent) = out
        _ <- ZIO.succeed(context.stack = newCurrent :: context.stack.tail)
      } yield result
    }
}
