package org.opentorah.xml

import zio.ZIO

// Type-safe XML attribute get/set - for use in DOM and SAX;
// inspired by JEuclid's net.sourceforge.jeuclid.context.Parameter and friends.
abstract class Attribute[T](
  val name: String,
  val namespace: Namespace,
  val default: T,
  val setDefault: Boolean
) extends Conversion[T] with Requireable[T] {

  require((name != null) && !name.contains(":"))
  require(name.nonEmpty || (namespace == Namespace.Xmlns))
  
  final override def equals(other: Any): Boolean = other match {
    case that: Attribute[_] => (name == that.name) && (namespace.getUri == that.namespace.getUri)
    case _ => false
  }

  final override def toString: String = s"$qName"

  final def qName: String = namespace.qName(name)

  final def inNamespace(namespace: Namespace): Attribute[T] =
    if (this.namespace != namespace) this else withNamespace(Namespace.No)

  def withNamespace(namespace: Namespace): Attribute[T]

  def getWithDefault(value: Option[T]): T = value.getOrElse(default)

  def doGet(value: Option[T]): T = value.get

  final def effectiveValue(value: Option[T]): Option[T] =
    if (!setDefault) value.filterNot(_ == default)
    else value.orElse(Some(default))

  // Value

  final def withValue(value: T): Attribute.Value[T] = withOptionalValue(Option(value))

  final def withOptionalValue(value: Option[T]): Attribute.Value[T] = new Attribute.Value[T](this, value)

  // Parser

  final override def optional: Parser[Option[T]] = Context.takeAttribute(this).flatMap { value =>
    value.fold[Parser[Option[T]]](ZIO.none)(parseFromString(_).map(Some(_)))
  }

  final def optionalOrDefault: Parser[T] = optional.map(_.getOrElse(default))

  final def toXml: Antiparser[T] = Antiparser(
    attributes = value => Seq(withValue(value))
  )

  final def toXmlOption: Antiparser[Option[T]] = Antiparser(
    attributes = value => Seq(withOptionalValue(value))
  )

  // Scala XML
  final def get(element: scala.xml.Elem): Option[T] = get(Xml.getAttribute(this, element))
  final def getWithDefault(element: scala.xml.Elem): T = getWithDefault(get(element))
  final def doGet(element: scala.xml.Elem): T = doGet(get(element))

  // DOM
  final def get(element: org.w3c.dom.Element): Option[T] = get(Dom.getAttribute(this, element))
  final def getWithDefault(element: org.w3c.dom.Element): T = getWithDefault(get(element))
  final def doGet(element: org.w3c.dom.Element): T = getWithDefault(get(element))

  // SAX
  final def get(attributes: org.xml.sax.Attributes): Option[T] = get(Sax.getAttribute(this, attributes))
  final def getWithDefault(attributes: org.xml.sax.Attributes): T = getWithDefault(get(attributes))
  final def doGet(attributes: org.xml.sax.Attributes): T = doGet(get(attributes))
}

object Attribute {

  final class StringAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: String = "",
    setDefault: Boolean = false
  ) extends Attribute[String](name, namespace, default, setDefault) with Conversion.StringConversion {

    override def withNamespace(namespace: Namespace): StringAttribute =
      new StringAttribute(name, namespace, default, setDefault)
  }

  def apply(
    name: String,
    namespace: Namespace = Namespace.No,
    default: String = "",
    setDefault: Boolean = false
  ): Attribute[String] = new StringAttribute(name, namespace, default, setDefault)

  final class BooleanAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Boolean = false,
    setDefault: Boolean = false
  ) extends Attribute[Boolean](name, namespace, default, setDefault) with Conversion.BooleanConversion {

    override def withNamespace(namespace: Namespace): BooleanAttribute =
      new BooleanAttribute(name, namespace, default, setDefault)

    def withSetDefault: BooleanAttribute =
      new BooleanAttribute(name, namespace, default, true)
  }

  final class IntAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Int = 0,
    setDefault: Boolean = false
  ) extends Attribute[Int](name, namespace, default, setDefault) with Conversion.IntConversion {

    override def withNamespace(namespace: Namespace): IntAttribute =
      new IntAttribute(name, namespace, default, setDefault)
  }

  final class PositiveIntAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Int = 1,
    setDefault: Boolean = false
  ) extends Attribute[Int](name, namespace, default, setDefault) with Conversion.PositiveIntConversion {

    override def withNamespace(namespace: Namespace): PositiveIntAttribute =
      new PositiveIntAttribute(name, namespace, default, setDefault)
  }

  final class FloatAttribute(
    name: String,
    namespace: Namespace = Namespace.No,
    default: Float = 0.0f,
    setDefault: Boolean = false
  ) extends Attribute[Float](name, namespace, default, setDefault) with Conversion.FloatConversion {

    override def withNamespace(namespace: Namespace): FloatAttribute =
      new FloatAttribute(name, namespace, default, setDefault)
  }

  final class Value[T](
    val attribute: Attribute[T],
    val value: Option[T]
  ) {
    override def toString: String = s"""$attribute="${valueToString.orNull}""""

    def effectiveValue: Option[T] = attribute.effectiveValue(value)

    def valueToString: Option[String] = effectiveValue.map(attribute.toString)

    def set(element: scala.xml.Elem): scala.xml.Elem =
      addAll(element, Seq(this))

    def set(element: org.w3c.dom.Element): Unit =
      effectiveValue.foreach(value => Dom.setAttribute(attribute, value, element))

    def set(attributes: org.xml.sax.helpers.AttributesImpl): Unit =
      effectiveValue.foreach(value => Sax.setAttribute(attribute, value, attributes))
  }

  val allAttributes: Parser[Seq[Value[String]]] = Context.takeAllAttributes

  // Scala XML
  def getAll(element: scala.xml.Elem): Seq[Attribute.Value[String]] = Xml.getAttributes(element)
  def setAll(element: scala.xml.Elem, attributes: Seq[Attribute.Value[_]]): scala.xml.Elem = Xml.setAttributes(element, attributes)
  def addAll(element: scala.xml.Elem, attributes: Seq[Attribute.Value[_]]): scala.xml.Elem = {
    val existing: Seq[Attribute.Value[_]] = Attribute.getAll(element)
    val toAdd: Seq[Attribute.Value[_]] = attributes
      .filterNot(toAdd => existing.exists(existing => existing.attribute.name == toAdd.attribute.name))

    Attribute.setAll(element, existing ++ toAdd)
  }

  // DOM
  def getAll(element: org.w3c.dom.Element): Seq[Attribute.Value[String]] = Dom.getAttributes(element)

  // SAX
  def getAll(attributes: org.xml.sax.Attributes): Seq[Attribute.Value[String]] = Sax.getAttributes(attributes)
}
