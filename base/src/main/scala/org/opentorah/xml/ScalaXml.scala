package org.opentorah.xml

import org.opentorah.util.Strings
import org.xml.sax.{InputSource, XMLFilter, XMLReader}

object ScalaXml extends Xml:

  override type Node = scala.xml.Node

  override type Attributes = scala.xml.Elem

  // Note: some whitespace is packaged not in Text, but in a different subclass of Atom[String], so:
  override type Text = scala.xml.Atom[?]

  override type Comment = scala.xml.Comment

  override def toString(node: Node): String = Strings.squashWhitespace(node match // TODO why the squash?
    case elem: Element => (elem.child map (_.text)).mkString(" ") // TODO hope this is not used: no tags, no attributes...

    // TODO is the rest any different from _.text?
    case text: Text => text.data.toString
    case special: scala.xml.SpecialNode => Strings.sbToString(special.buildString)
    case node: Node => node.text
  )

  override protected def loadFromInputSource(
    inputSource: InputSource,
    filters: Seq[XMLFilter],
    resolver: Option[Resolver]
  ): Element =

    val adapter: scala.xml.parsing.FactoryAdapter = scala.xml.parsing.NoBindingFactoryAdapter()
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

  override def isText(node: Node): Boolean = node.isInstanceOf[Text]
  override def asText(node: Node): Text    = node.asInstanceOf[Text]
  override def getText(text: Text): String = text.text

  override def mkText(text: String, seed: Node): Text = mkText(text)
  def mkText(text: String): Text = scala.xml.Text(text)

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

  private given CanEqual[scala.xml.NamespaceBinding, scala.xml.NamespaceBinding] = CanEqual.derived

  // Note: maybe support re-definitions of the namespace bindings - like in  scala.xml.NamespaceBinding.shadowRedefined()?
  override def getNamespaces(attributes: Attributes): Seq[Namespace] =
    @scala.annotation.tailrec
    def get(result: Seq[Namespace], namespaceBinding: scala.xml.NamespaceBinding): Seq[Namespace] =
      if namespaceBinding == scala.xml.TopScope then result else
        val namespace: Namespace = Namespace(
          prefix = namespaceBinding.prefix,
          uri = namespaceBinding.uri
        )
        get(result :+ namespace, namespaceBinding.parent)

    get(Seq.empty, attributes.scope)

  // TODO implement
  override def isNamespaceDeclared(namespace: Namespace, attributes: Attributes): Boolean = ???

  override def declareNamespace(namespace: Namespace, element: Element): Element =
    element.copy(scope = scala.xml.NamespaceBinding(namespace.getPrefix.orNull, namespace.getUri.get, element.scope))

  override protected def getAttributeValueString(attribute: Attribute[?], attributes: Attributes): Option[String] = {
    val name: String = attribute.name
    val namespace: Namespace = attribute.namespace
    if namespace.isDefault then attributes.attribute(name)
    else attributes.attribute(namespace.uri, name)
  }.map(_.text)

  override def getAttributes(attributes: Attributes): Attribute.StringValues = attributes.attributes.toSeq
    .filter(_.isInstanceOf[scala.xml.Attribute])
    .map(_.asInstanceOf[scala.xml.Attribute])
    .map(attribute =>
      val namespace: Namespace = Namespace(
        prefix = attribute.pre,
        uri = attribute.getNamespace(attributes)
      )
      Attribute(
        name = attribute.key,
        namespace = namespace
      ).optional.withValue(Option(attribute.value).map(_.text))
    )

  // TODO addAll() doesn't modify existing attributes; this should...
  override protected def setAttribute[T](attribute: Attribute[T], value: T, element: Element): Element =
    addAttributes(Seq(attribute.required.withValue(value)), element)

  override def setAttributes(attributes: Attribute.Values, element: Element): Element =
    element.copy(attributes = attributes.foldRight[scala.xml.MetaData](scala.xml.Null)(
      (attributeValue, next) => toMetaData(attributeValue, next))
    )

  private def toMetaData[T](attributeValue: Attribute.Value[T], next: scala.xml.MetaData): scala.xml.MetaData =
    val attribute: Attribute[T] = attributeValue.attribute
    val value: Option[T] = attributeValue.effectiveValue
    scala.xml.Attribute(
      pre = attribute.namespace.getPrefix.orNull,
      key = attribute.name,
      value = value.map(attribute.toString).map(mkText).map(Seq(_)).orNull,
      next = next
    )

  override def getChildren(element: Element): Nodes = element.child

  override def setChildren(element: Element, children: Nodes): Element = element.copy(child = children)

  override protected def descendats(nodes: Nodes, elementName: String): Nodes = nodes.flatMap(node => node.flatMap(_ \\ elementName))

  def toNodes(nodes: Nodes): Element.Nodes = Element.Nodes(ScalaXml)(nodes)