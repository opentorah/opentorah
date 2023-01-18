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

  final override def toString: String = qName

  final def qName: String = namespace.qName(name)

  final def optional : Attribute.Optional [T] = Attribute.Optional [T](this)
  final def orDefault: Attribute.OrDefault[T] = Attribute.OrDefault[T](this)
  final def required : Attribute.Required [T] = Attribute.Required [T](this)

  // TODO ZIOify! (and unify Effects.effect() and IO.succeed() - and then remove overrides of one of the methods and it itself...Z)
  protected def fromString(value: String): T
  protected def parseFromString(value: String): Effects.IO[T] = Effects.effect(fromString(value))
  protected def toString(value: T): String

object Attribute:

  final class Value[T](
    val attributeParsable: Parsable[T, ?],
    val value: Option[T]
  )(using CanEqual[T, T]):
    def attribute: Attribute[T] = attributeParsable.attribute

    override def toString: String = s"""$attribute="${valueEffective.getOrElse("<default>")}""""

    def set(xml: XmlAttributes)(element: xml.Element): xml.Element = xml.setAttribute(this, element)

    def valueEffective: Option[String] =
      val result: Option[T] =
        if !attributeParsable.isSetDefault
        then value.filterNot(_ == attribute.default)
        else value.orElse(Some(attribute.default))
      result.map(attribute.toString)

  type Values = Seq[Value[?]]

  type StringValues = Seq[Value[String]]

  def allAttributes: Parser[StringValues] = Parsing.allAttributes

  // TODO A is either T or Option[T]; are there any type-level tricks I can use to enforce this?
  sealed abstract class Parsable[T, A](val attribute: Attribute[T])(using CanEqual[T, T]) extends org.opentorah.xml.Parsable[A]:
    final override protected def parser: Parser[A] = toParser(Parsing.takeAttribute(attribute).flatMap {
      case None => zio.ZIO.none
      case Some(value) => attribute.parseFromString(value).map(Some(_))
    })

    final def get(xml: XmlAttributes)(attributes: xml.Attributes): A =
      fromOption(xml.getAttribute(attribute, attributes).filter(_.nonEmpty).map(attribute.fromString))

    final protected def orDefault(value: Option[T]): T = value.getOrElse(attribute.default)

    final override def unparser: Unparser[A] = Unparser(attributes = value => Seq(withValue(value)))

    final def withValueOption(value: Option[T]): Value[T] = Attribute.Value[T](this, value)
    final def withValue(value: A): Value[T] = withValueOption(toOption(value))

    def isSetDefault: Boolean

    protected def toParser: Parser[Option[T]] => Parser[A]

    protected val fromOption: Option[T] => A
    
    protected val toOption: A => Option[T]

  final class Optional[T](attribute: Attribute[T])(using CanEqual[T, T]) extends Parsable[T, Option[T]](attribute):
    override def isSetDefault: Boolean = false
    override protected val toParser: Parser[Option[T]] => Parser[Option[T]] = identity
    override protected val fromOption: Option[T] => Option[T] = identity
    override protected val toOption: Option[T] => Option[T] = identity

  final class OrDefault[T](attribute: Attribute[T])(using CanEqual[T, T]) extends Parsable[T, T](attribute):
    override def isSetDefault: Boolean = false
    override protected val toParser: Parser[Option[T]] => Parser[T] = _.map(orDefault)
    override protected val fromOption: Option[T] => T = orDefault
    override protected val toOption: T => Option[T] = Some(_)

  final class Required[T](attribute: Attribute[T])(using CanEqual[T, T]) extends Parsable[T, T](attribute):
    override def isSetDefault: Boolean = true
    override protected val toParser: Parser[Option[T]] => Parser[T] = Effects.required(_, attribute)
    override protected val fromOption: Option[T] => T = _.get // TODO better error handling
    override protected val toOption: T => Option[T] = Some(_)

  final class StringAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: String = ""
  ) extends Attribute[String](name, namespace, default):
    override protected def fromString(value: String): String = value
    override protected def toString(value: String): String = value
    override protected def parseFromString(value: String): Effects.IO[String] = zio.ZIO.succeed(value)

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
    override protected def toString(value: Boolean): String = value.toString
    override protected def fromString(value: String): Boolean = value match
      case "yes" => true
      case "no"  => false
      case value => value.toBoolean

  final class IntAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Int = 0
  ) extends Attribute[Int](name, namespace, default):
    override protected def fromString(value: String): Int = int(value, mustBePositive = false)
    override protected def toString(value: Int): String = value.toString

  final class PositiveIntAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Int = 1
  ) extends Attribute[Int](name, namespace, default):
    override protected def fromString(value: String): Int = int(value, mustBePositive = true)
    override protected def toString(value: Int): String = value.toString

  private def int(value: String, mustBePositive: Boolean): Int =
    val result: Int = value.toInt
    require(!mustBePositive || (result > 0), s"Non-positive integer: $result")
    result

  final class FloatAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Float = 0.0f
  ) extends Attribute[Float](name, namespace, default):
    override protected def fromString(value: String): Float = value.toFloat
    override protected def toString(value: Float): String = value.toString
  
  final class EnumeratedAttribute[T](
    name: String,
    namespace: Namespace = Namespace.No,
    default: T,
    values: Seq[T],
    getName: T => String
  )(using CanEqual[T, T]) extends Attribute[T](name, namespace, default):
    override protected def toString(value: T): String = getName(value)
    override protected def fromString(value: String): T =
      values.find(getName(_) == value).getOrElse(throw IllegalArgumentException(s"Unknown $name: $value"))
