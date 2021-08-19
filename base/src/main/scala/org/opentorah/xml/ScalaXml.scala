package org.opentorah.xml

import org.opentorah.util.Strings
import org.xml.sax.{InputSource, XMLFilter, XMLReader}
import zio.{URIO, ZIO}

object ScalaXml extends Xml {

  override type Node = scala.xml.Node

  override type Attributes = scala.xml.Elem

  // Note: some whitespace is packaged not in Text, but in a different subclass of Atom[String], so:
  override type Text = scala.xml.Atom[_]

  override type Comment = scala.xml.Comment

  // TODO up into the Model - after .copy(child = children) is abstracted...
  final class Transform[R](transform: Element => URIO[R, Element]) {
    val one: Element => URIO[R, Element] = element => for {
      newElement <- transform(element)
      children <- URIO.foreach(getChildren(newElement)) {
        node => if (!isElement(node)) URIO.succeed(node) else one(asElement(node))
      }
    } yield newElement.copy(child = children)

    val all: Seq[Element] => URIO[R, Seq[Element]] = URIO.foreach(_)(one)
  }

  override def toString(node: Node): String = Strings.squashWhitespace(node match { // TODO why the squash?
    case elem: Element => (elem.child map (_.text)).mkString(" ") // TODO hope this is not used: no tags, no attributes...

    // TODO is the rest any different from _.text?
    case text: Text => text.data.toString
    case special: scala.xml.SpecialNode => Strings.sbToString(special.buildString)
    case node: Node => node.text
  })

  override protected def loadFromSource(
    inputSource: InputSource,
    filters: Seq[XMLFilter],
    resolver: Option[Resolver]
  ): Element = {

    val adapter: scala.xml.parsing.FactoryAdapter = new scala.xml.parsing.NoBindingFactoryAdapter()
    adapter.scopeStack = scala.xml.TopScope :: adapter.scopeStack

    // Note: scala.xml.parsing.FactoryAdapter looks for namespace declarations
    // in the attributes passed into startElement(), and does not override
    // startPrefixMapping(), so it does not work with a namespace-aware parser...
    val xmlReader: XMLReader = Xerces.getXMLReader(filters, resolver, xincludeAware = false)
    xmlReader.setContentHandler(adapter)
    xmlReader.setDTDHandler(adapter)

    xmlReader.parse(inputSource)

    adapter.scopeStack = adapter.scopeStack.tail
    adapter.rootElem.asInstanceOf[Element]
  }

  override def isText(node: Node): Boolean = node.isInstanceOf[Text]
  override def asText(node: Node): Text    = node.asInstanceOf[Text]
  override def getText(text: Text): String = text.text

  override def mkText(text: String, seed: Node): Text = mkText(text)
  def mkText(text: String): Text = new scala.xml.Text(text)

  def mkComment(text: String, seed: Node): Comment = mkComment(text)
  def mkComment(text: String): Comment = scala.xml.Comment(text)

  override def isElement(node: Node): Boolean = node.isInstanceOf[Element]
  override def asElement(node: Node): Element = node.asInstanceOf[Element]

  override def getName(element: Element): String = element.label
  override def getPrefix(element: Element): Option[String] = Option(element.prefix)

  override def getNamespace(attributes: Attributes): Namespace = Namespace(
    prefix = attributes.prefix,
    uri = attributes.getNamespace(attributes.prefix)
  )

  // Note: maybe support re-definitions of the namespace bindings - like in  scala.xml.NamespaceBinding.shadowRedefined()?
  override def getNamespaces(attributes: Attributes): Seq[Namespace] = {
    @scala.annotation.tailrec
    def get(result: Seq[Namespace], namespaceBinding: scala.xml.NamespaceBinding): Seq[Namespace] =
      if (namespaceBinding == scala.xml.TopScope) result else {
        val namespace: Namespace = Namespace(
          prefix = namespaceBinding.prefix,
          uri = namespaceBinding.uri
        )
        get(result :+ namespace, namespaceBinding.parent)
      }

    get(Seq.empty, attributes.scope)
  }

  // TODO implement
  override def isNamespaceDeclared(namespace: Namespace, attributes: Attributes): Boolean = ???

  override def declareNamespace(namespace: Namespace, element: Element): Element =
    element.copy(scope = scala.xml.NamespaceBinding(namespace.getPrefix.orNull, namespace.getUri.get, element.scope))

  override protected def getAttributeValueString(attribute: Attribute[_], attributes: Attributes): Option[String] = {
    val name: String = attribute.name
    val namespace: Namespace = attribute.namespace
    if (namespace.isDefault) attributes.attribute(name)
    else attributes.attribute(namespace.uri, name)
  }.map(_.text)

  override def getAttributes(attributes: Attributes): Seq[Attribute.Value[String]] = attributes.attributes.toSeq
    .filter(_.isInstanceOf[scala.xml.Attribute])
    .map(_.asInstanceOf[scala.xml.Attribute])
    .map { attribute =>
      val namespace: Namespace = Namespace(
        prefix = attribute.pre,
        uri = attribute.getNamespace(attributes)
      )
      Attribute(
        name = attribute.key,
        namespace = namespace
      ).optional.withValue(Option(attribute.value).map(_.text))
    }

  // TODO addAll() doesn't modify existing attributes; this should...
  override protected def setAttribute[T](attribute: Attribute[T], value: T, element: Element): Element =
    addAttributes(Seq(attribute.required.withValue(value)), element)

  override def setAttributes(attributes: Attribute.Values, element: Element): Element =
    element.copy(attributes = attributes.foldRight[scala.xml.MetaData](scala.xml.Null)(
      (attributeValue, next) => toMetaData(attributeValue, next))
    )

  private def toMetaData[T](attributeValue: Attribute.Value[T], next: scala.xml.MetaData): scala.xml.MetaData = {
    val attribute: Attribute[T] = attributeValue.attribute
    val value: Option[T] = attributeValue.effectiveValue
    scala.xml.Attribute(
      pre = attribute.namespace.getPrefix.orNull,
      key = attribute.name,
      value = value.map(attribute.toString).map(mkText).map(Seq(_)).orNull,
      next = next
    )
  }

  override def getChildren(element: Element): Nodes = element.child

  // TODO up into Xml - after parsing is abstracted over Xml...
  def descendants[T](nodes: Nodes, elementName: String, elements: Elements[T]): Parser[Seq[T]] = ZIO.foreach(
    nodes.flatMap(node => node.flatMap(_ \\ elementName).filter(isElement).map[Element](asElement))
  )(descendant => elements.parse(From.xml("descendants", descendant)))

  // TODO up into Xml - after the seed situation is clarified...
  def multi(nodes: Nodes, separator: String = ", "): Nodes = nodes match {
    case Nil => Nil
    case n :: Nil => Seq(n)
    case n :: n1 :: ns if isElement(n) && isElement(n1) => Seq(n, mkText(separator)) ++ multi(n1 :: ns, separator)
    case n :: ns => Seq(n) ++ multi(ns, separator)
    case n => n
  }
}
