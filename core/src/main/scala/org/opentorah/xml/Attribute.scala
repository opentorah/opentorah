package org.opentorah.xml

import org.opentorah.util.Effects

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

  final override def toString: String = s"$qName"

  final def qName: String = namespace.qName(name)

  final def optional           : Attribute.Optional [T] = Attribute.Optional [T](this, setDefault = false)
  final def optionalSetDefault : Attribute.Optional [T] = Attribute.Optional [T](this, setDefault = true )
  final def orDefault          : Attribute.OrDefault[T] = Attribute.OrDefault[T](this, setDefault = false)
  final def orDefaultSetDefault: Attribute.OrDefault[T] = Attribute.OrDefault[T](this, setDefault = true )
  final def required           : Attribute.Required [T] = Attribute.Required [T](this)

  def toString(value: T): String = value.toString

  final def get(value: Option[String]): Option[T] = value.filter(_.nonEmpty).map(fromString)

  // TODO ZIOify! (and unify Effects.effect() and IO.succeed() - and then remove overrides of one of the methods and it itself...Z)
  def fromString(value: String): T
  def parseFromString(value: String): Effects.IO[T] = Effects.effect(fromString(value))

object Attribute:

  final class Value[T](
    val attributeParsable: Parsable[T, ?],
    val value: Option[T]
  ):
    def attribute: Attribute[T] = attributeParsable.attribute

    override def toString: String = s"""$attribute="${valueToString.orNull}""""

    def effectiveValue: Option[T] = attributeParsable.effectiveValue(value)

    def valueToString: Option[String] = effectiveValue.map(attribute.toString)

    def set(xml: XmlAttributes)(element: xml.Element): xml.Element = xml.setAttribute(this, element)

  type Values = Seq[Value[?]]

  type StringValues = Seq[Value[String]]

  def allAttributes: Parser[StringValues] = Parsing.allAttributes

  sealed abstract class Parsable[T, A](val attribute: Attribute[T]) extends org.opentorah.xml.Parsable[A]:
    final override def unparser: Unparser[A] = Unparser(
      attributes = value => Seq(withValue(value))
    )

    def withValue(value: A): Value[T]

    def effectiveValue(value: Option[T]): Option[T]

    def get(xml: XmlAttributes)(element: xml.Attributes): A

  private def optionalParser[T](attribute: Attribute[T]): Parser[Option[T]] =
    Parsing.takeAttribute(attribute).flatMap(value =>
      value.fold[Parser[Option[T]]](zio.ZIO.none)(attribute.parseFromString(_).map(Some(_)))
    )

  final class Optional[T](attribute: Attribute[T], setDefault: Boolean)(using CanEqual[T, T]) extends Parsable[T, Option[T]](attribute):
    override protected def parser: Parser[Option[T]] = optionalParser(attribute)
    override def withValue(value: Option[T]): Value[T] = Attribute.Value[T](this, value)
    override def effectiveValue(value: Option[T]): Option[T] =
      if !setDefault then value.filterNot(_ == attribute.default)
      else value.orElse(Some(attribute.default))

    override def get(xml: XmlAttributes)(element: xml.Attributes): Option[T] = xml.getAttribute(attribute, element)

  final class OrDefault[T](attribute: Attribute[T], setDefault: Boolean)(using CanEqual[T, T]) extends Parsable[T, T](attribute):
    override protected def parser: Parser[T] = optionalParser(attribute).map(_.getOrElse(attribute.default))
    override def withValue(value: T): Attribute.Value[T] = Attribute.Value[T](this, Option(value))
    override def effectiveValue(value: Option[T]): Option[T] =
      if !setDefault then value.filterNot(_ == attribute.default)
      else value.orElse(Some(attribute.default))

    override def get(xml: XmlAttributes)(element: xml.Attributes): T = xml.getAttributeWithDefault(attribute, element)

  final class Required[T](attribute: Attribute[T]) extends Parsable[T, T](attribute):
    override protected def parser: Parser[T] = Effects.required(optionalParser(attribute), attribute)
    override def withValue(value: T): Attribute.Value[T] = Attribute.Value[T](this, Option(value))
    override def effectiveValue(value: Option[T]): Option[T] = value.orElse(Some(attribute.default))

    override def get(xml: XmlAttributes)(element: xml.Attributes): T = xml.doGetAttribute(attribute, element)

  final class StringAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: String = ""
  ) extends Attribute[String](name, namespace, default):
    final override def fromString(value: String): String = value

    final override def parseFromString(value: String): Effects.IO[String] = zio.ZIO.succeed(value)

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
    final override def fromString(value: String): Boolean = value match
      case "yes" => true
      case "no"  => false
      case value => value.toBoolean

  final class IntAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Int = 0
  ) extends Attribute[Int](name, namespace, default):
    final override def fromString(value: String): Int = int(value, mustBePositive = false)

  final class PositiveIntAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Int = 1
  ) extends Attribute[Int](name, namespace, default):
    final override def fromString(value: String): Int = int(value, mustBePositive = true)

  private def int(value: String, mustBePositive: Boolean): Int =
    val result = value.toInt
    if mustBePositive && result <= 0 then throw IllegalArgumentException(s"Non-positive integer: $result")
    result

  final class FloatAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Float = 0.0f
  ) extends Attribute[Float](name, namespace, default):
    final override def fromString(value: String): Float = value.toFloat
