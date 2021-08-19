package org.opentorah.xml

import org.opentorah.util.Effects
import zio.ZIO

// Type-safe XML attribute get/set - for use in DOM and SAX;
// inspired by JEuclid's net.sourceforge.jeuclid.context.Parameter and friends.
abstract class Attribute[T](
  val name: String,
  val namespace: Namespace,
  val default: T
) extends Conversion[T] {

  require((name != null) && !name.contains(":"))
  require(name.nonEmpty || (namespace == Namespace.Xmlns))
  
  final override def equals(other: Any): Boolean = other match {
    case that: Attribute[_] => (name == that.name) && (namespace.getUri == that.namespace.getUri)
    case _ => false
  }

  final override def toString: String = s"$qName"

  final def qName: String = namespace.qName(name)

  final def optional           : Attribute.Optional [T] = new Attribute.Optional [T](this, setDefault = false)
  final def optionalSetDefault : Attribute.Optional [T] = new Attribute.Optional [T](this, setDefault = true )
  final def orDefault          : Attribute.OrDefault[T] = new Attribute.OrDefault[T](this, setDefault = false)
  final def orDefaultSetDefault: Attribute.OrDefault[T] = new Attribute.OrDefault[T](this, setDefault = true )
  final def required           : Attribute.Required [T] = new Attribute.Required [T](this)
}

object Attribute {

  final class Value[T](
    val attributeParsable: Parsable[T, _],
    val value: Option[T]
  ) {
    def attribute: Attribute[T] = attributeParsable.attribute

    override def toString: String = s"""$attribute="${valueToString.orNull}""""

    def effectiveValue: Option[T] = attributeParsable.effectiveValue(value)

    def valueToString: Option[String] = effectiveValue.map(attribute.toString)

    def set(xml: XmlAttributes)(element: xml.Element): xml.Element = xml.setAttribute(this, element)
  }

  type Values = Seq[Value[_]]

  def allAttributes: Parser[Seq[Value[String]]] = Context.allAttributes

  sealed abstract class Parsable[T, A](val attribute: Attribute[T]) extends org.opentorah.xml.Parsable[A] {
    final override def unparser: Unparser[A] = Unparser(
      attributes = value => Seq(withValue(value))
    )

    def withValue(value: A): Value[T]

    def effectiveValue(value: Option[T]): Option[T]

    def get(xml: XmlAttributes)(element: xml.Attributes): A
  }

  private def optionalParser[T](attribute: Attribute[T]): Parser[Option[T]] =
    Context.takeAttribute(attribute).flatMap { value =>
      value.fold[Parser[Option[T]]](ZIO.none)(attribute.parseFromString(_).map(Some(_)))
    }

  final class Optional[T](attribute: Attribute[T], setDefault: Boolean) extends Parsable[T, Option[T]](attribute) {
    override protected def parser: Parser[Option[T]] = optionalParser(attribute)
    override def withValue(value: Option[T]): Value[T] = new Attribute.Value[T](this, value)
    override def effectiveValue(value: Option[T]): Option[T] =
      if (!setDefault) value.filterNot(_ == attribute.default)
      else value.orElse(Some(attribute.default))

    override def get(xml: XmlAttributes)(element: xml.Attributes): Option[T] = xml.getAttribute(attribute, element)
  }

  final class OrDefault[T](attribute: Attribute[T], setDefault: Boolean) extends Parsable[T, T](attribute) {
    override protected def parser: Parser[T] = optionalParser(attribute).map(_.getOrElse(attribute.default))
    override def withValue(value: T): Attribute.Value[T] = new Attribute.Value[T](this, Option(value))
    override def effectiveValue(value: Option[T]): Option[T] =
      if (!setDefault) value.filterNot(_ == attribute.default)
      else value.orElse(Some(attribute.default))

    override def get(xml: XmlAttributes)(element: xml.Attributes): T = xml.getAttributeWithDefault(attribute, element)
  }

  final class Required[T](attribute: Attribute[T]) extends Parsable[T, T](attribute) {
    override protected def parser: Parser[T] = Effects.required(optionalParser(attribute), attribute)
    override def withValue(value: T): Attribute.Value[T] = new Attribute.Value[T](this, Option(value))
    override def effectiveValue(value: Option[T]): Option[T] = value.orElse(Some(attribute.default))

    override def get(xml: XmlAttributes)(element: xml.Attributes): T = xml.doGetAttribute(attribute, element)
  }

  final class StringAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: String = ""
  ) extends Attribute[String](name, namespace, default) with Conversion.StringConversion

  def apply(
    name: String,
    namespace: Namespace = Namespace.No,
    default: String = ""
  ): Attribute[String] = new StringAttribute(name, namespace, default)

  final class BooleanAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Boolean = false
  ) extends Attribute[Boolean](name, namespace, default) with Conversion.BooleanConversion

  final class IntAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Int = 0
  ) extends Attribute[Int](name, namespace, default) with Conversion.IntConversion

  final class PositiveIntAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Int = 1
  ) extends Attribute[Int](name, namespace, default) with Conversion.PositiveIntConversion

  final class FloatAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Float = 0.0f
  ) extends Attribute[Float](name, namespace, default) with Conversion.FloatConversion
}
