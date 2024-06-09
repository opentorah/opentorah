package org.opentorah.xml

import org.opentorah.util.Effects
import zio.ZIO

// Type-safe XML attribute get/set - for use in DOM and SAX;
// inspired by JEuclid's net.sourceforge.jeuclid.context.Parameter and friends.
abstract class Attribute[T](
  val name: String,
  val namespace: Namespace,
  val default: T
)(using CanEqual[T, T]) derives CanEqual:
  require((name != null) && !name.contains(":"))
  require(name.nonEmpty || (namespace == Namespace.Xmlns))

  final override def equals(other: Any): Boolean =
    val that: Attribute[T] = other.asInstanceOf[Attribute[T]]
    (name == that.name) && (namespace.getUri == that.namespace.getUri)

  final override def toString: String = qName

  final def qName: String = namespace.qName(name)

  final def optional : Attribute.Optional [T] = Attribute.Optional [T](this)
  final def orDefault: Attribute.OrDefault[T] = Attribute.OrDefault[T](this)
  final def required : Attribute.Required [T] = Attribute.Required [T](this)

  protected def toString(value: T): String = value.toString
  protected def fromString(value: String): Effects.IO[T]

  protected final def fromString(toOption: String => Option[T])(value: String): ZIO[Any, Effects.Error, T] = toOption(value) match
    case Some(result) => ZIO.succeed(result)
    case None => ZIO.fail(Effects.Error(s"Invalid value for attribute $this: $value"))

  final def get(element: Xml.Element): Option[String] = {
    if namespace.isDefault then element.attribute(name)
    else element.attribute(namespace.uri, name)
  }.map(_.text)
  
  final def remove(element: Xml.Element): Xml.Element =
    Attribute.set(Attribute.get(element).filterNot(_.attribute.name == name), element)

object Attribute:

  final class Value[T](
    val attributeParsable: Parsable[T, ?],
    val value: Option[T]
  )(using CanEqual[T, T]):
    def attribute: Attribute[T] = attributeParsable.attribute

    override def toString: String = s"""$attribute="${valueEffective.getOrElse("<default>")}""""

    // TODO rework
    def add(element: Xml.Element): Xml.Element = Attribute.add(Seq(this), element)

    // TODO add() doesn't modify existing attributes; this should...
    def set(element: Xml.Element): Xml.Element = valueEffective match
      case None => element
      case _ => Attribute.add(Seq(this), element)
      
    def valueEffective: Option[String] =
      val result: Option[T] =
        if !attributeParsable.isSetDefault
        then value.filterNot(_ == attribute.default)
        else value.orElse(Some(attribute.default))
      result.map(attribute.toString)

  type Values = Seq[Value[?]]

  type StringValues = Seq[Value[String]]

  def allAttributes: Parser[StringValues] = Parsing.allAttributes

  def get(element: Xml.Element): StringValues = element.attributes.toSeq
    .filter(_.isInstanceOf[scala.xml.Attribute])
    .map(_.asInstanceOf[scala.xml.Attribute])
    .map(attribute =>
      Attribute(
        name = attribute.key,
        namespace = Namespace(
          prefix = attribute.pre,
          uri = attribute.getNamespace(element)
        )
      ).optional.withValue(Option(attribute.value).map(_.text))
    )
  
  // TODO rework
  final def add(attributes: Values, element: Xml.Element): Xml.Element =
    val existing: Attribute.Values = get(element)
    val toAdd: Attribute.Values = attributes
      .filterNot(toAdd => existing.exists(existing => existing.attribute.name == toAdd.attribute.name))

    set(existing ++ toAdd, element)

  def set(attributes: Attribute.Values, element: Xml.Element): Xml.Element =
    element.copy(attributes = attributes.foldRight[scala.xml.MetaData](scala.xml.Null)(
      (attributeValue, next) => toMetaData(attributeValue, next))
    )

  private def toMetaData[T](attributeValue: Attribute.Value[T], next: scala.xml.MetaData): scala.xml.MetaData =
    val attribute: Attribute[T] = attributeValue.attribute
    scala.xml.Attribute(
      pre = attribute.namespace.getPrefix.orNull,
      key = attribute.name,
      value = attributeValue.valueEffective.map(Xml.mkText).map(Seq(_)).orNull,
      next = next
    )  
    
  // TODO A is either T or Option[T]; are there any type-level tricks I can use to enforce this?
  sealed abstract class Parsable[T, A](val attribute: Attribute[T])(using CanEqual[T, T]) extends org.opentorah.xml.Parsable[A]:
    final override protected def parser: Parser[A] =
      Parsing.takeAttribute(attribute).flatMap(fromStringOption)

    final def get(element: Xml.Element): Effects.IO[A] =
      fromStringOption(attribute.get(element).filter(_.nonEmpty))
    
    private def fromStringOption(stringOption: Option[String]): ZIO[Any, Effects.Error, A] = for
      option: Option[T] <- stringOption match
        case None => ZIO.none
        case Some(value) => attribute.fromString(value).map(Some(_))
      result <- fromOption(option)
    yield result

    final override def unparser: Unparser[A] = Unparser(attributes = value => Seq(withValue(value)))

    final def withValueOption(value: Option[T]): Value[T] = Attribute.Value[T](this, value)
    final def withValue(value: A): Value[T] = withValueOption(toOption(value))

    def isSetDefault: Boolean

    protected def fromOption(option: Option[T]): Effects.IO[A]
    
    protected val toOption: A => Option[T]

  final class Optional[T](attribute: Attribute[T])(using CanEqual[T, T]) extends Parsable[T, Option[T]](attribute):
    override def isSetDefault: Boolean = false
    override protected val toOption: Option[T] => Option[T] = identity
    override protected def fromOption(option: Option[T]): Effects.IO[Option[T]] = ZIO.succeed(option)

  final class OrDefault[T](attribute: Attribute[T])(using CanEqual[T, T]) extends Parsable[T, T](attribute):
    override def isSetDefault: Boolean = false
    override protected val toOption: T => Option[T] = Some(_)
    override protected def fromOption(option: Option[T]): Effects.IO[T] = ZIO.succeed(option.getOrElse(attribute.default))

  final class Required[T](attribute: Attribute[T])(using CanEqual[T, T]) extends Parsable[T, T](attribute):
    override def isSetDefault: Boolean = true
    override protected val toOption: T => Option[T] = Some(_)
    override protected def fromOption(option: Option[T]): Effects.IO[T] = Effects.required(ZIO.succeed(option), attribute)

  private final class StringAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: String = ""
  ) extends Attribute[String](name, namespace, default):
    override protected def fromString(value: String): Effects.IO[String] = ZIO.succeed(value)

  def apply(
    name: String,
    namespace: Namespace = Namespace.No,
    default: String = ""
  ): Attribute[String] = StringAttribute(name, namespace, default)

  final class BooleanAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Boolean = false
  ) extends Attribute[Boolean](name, namespace, default):
    override protected def fromString(value: String): Effects.IO[Boolean] = value match
      case "yes" => ZIO.succeed(true)
      case "no"  => ZIO.succeed(false)
      case value => fromString(_.toBooleanOption)(value)

  abstract class IntAttributeBase(
    name: String,
    namespace: Namespace,
    default: Int
  ) extends Attribute[Int](name, namespace, default):
    protected final def int(value: String, mustBePositive: Boolean): Effects.IO[Int] = for
      n <- fromString(_.toIntOption)(value)
      result <-
        if !mustBePositive || (n > 0)
        then ZIO.succeed(n)
        else ZIO.fail(Effects.Error(s"Non-positive integer: $n"))
    yield result

  final class IntAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Int = 0
  ) extends IntAttributeBase(name, namespace, default):
    override protected def fromString(value: String): Effects.IO[Int] = int(value, mustBePositive = false)

  final class PositiveIntAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Int = 1
  ) extends IntAttributeBase(name, namespace, default):
    override protected def fromString(value: String): Effects.IO[Int] = int(value, mustBePositive = true)

  final class FloatAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Float = 0.0f
  ) extends Attribute[Float](name, namespace, default):
    override protected def fromString(value: String): Effects.IO[Float] = fromString(_.toFloatOption)(value)

  final class EnumeratedAttribute[T](
    name: String,
    namespace: Namespace = Namespace.No,
    default: T,
    values: Seq[T],
    getName: T => String
  )(using CanEqual[T, T]) extends Attribute[T](name, namespace, default):
    override protected def toString(value: T): String = getName(value)
    override protected def fromString(value: String): Effects.IO[T] =
      fromString(value => values.find(getName(_) == value))(value)
