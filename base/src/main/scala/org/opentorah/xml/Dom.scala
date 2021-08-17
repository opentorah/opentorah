package org.opentorah.xml

import org.xml.sax.{InputSource, XMLFilter}

// Note: declareNamespace() and setAttribute[s]() modify in-place.
object Dom extends Model {

  override type Node = org.w3c.dom.Node
  override type PreElement = org.w3c.dom.Element
  override type Text = org.w3c.dom.Text
  override type Comment = org.w3c.dom.Comment

  override protected def loadFromSource(
    inputSource: InputSource,
    filters: Seq[XMLFilter],
    resolver: Option[Resolver]
  ): Element = {
    val result: javax.xml.transform.dom.DOMResult = new javax.xml.transform.dom.DOMResult

    Saxon.Saxon10.transform(
      filters = filters,
      resolver = resolver,
      stylesheetFile = None,
      inputSource = inputSource,
      result
    )

    result.getNode.asInstanceOf[org.w3c.dom.Document].getDocumentElement
  }

  override def isText(node: Node): Boolean = node.isInstanceOf[Text]
  override def asText(node: Node): Text =    node.asInstanceOf[Text]
  override def getText(text: Text): String = text.getData
  override def mkText(text: String, seed: Node): Text = seed.getOwnerDocument.createTextNode(text)

  def mkComment(text: String, seed: Node): Comment = seed.getOwnerDocument.createComment(text)

  override def toString(node: Node): String = node match {
    case comment: org.w3c.dom.Comment => s"<!--${comment.getTextContent}-->"
    case node: Node => node.getTextContent
  }

  override def isElement(node: Node): Boolean = node.isInstanceOf[Element]
  override def asElement(node: Node): Element = node.asInstanceOf[Element]
  override def getName(element: Element): String = element.getLocalName
  override def getPrefix(element: Element): Option[String] = Option(element.getPrefix)

  override def getNamespace(element: Element): Namespace = Namespace(
    uri = element.getNamespaceURI,
    prefix = element.getPrefix
  )

  override def getNamespaces(element: Element): Seq[Namespace] = {
    val list: org.w3c.dom.NamedNodeMap = element.getAttributes
    for {
      index <- 0 until list.getLength
      attribute = list.item(index).asInstanceOf[org.w3c.dom.Attr]
      if attribute.getNamespaceURI == Namespace.Xmlns.uri
      localName = attribute.getLocalName
      prefix = attribute.getPrefix
    } yield Namespace(
      uri = Option(attribute.getValue),
      prefix =
        if (localName == Namespace.Xmlns.prefix) {
          require(prefix == null)
          None
        } else {
          require(prefix == Namespace.Xmlns.prefix)
          require((localName != null) && localName.nonEmpty)
          Some(localName)
        }
    )
  }

  override def isNamespaceDeclared(namespace: Namespace, element: Element): Boolean =
    namespace.attribute.get(element) == namespace.getUri

  override def declareNamespace(namespace: Namespace, element: Element): Element = {
    namespace.attributeValue.set(element)
    element
  }

  override protected def getAttributeValueString(attribute: Attribute[_], element: Element): Option[String] = {
    val name: String = attribute.name
    val namespace: Namespace = attribute.namespace
    Option(
      if (namespace.isDefault) element.getAttribute(name)
      else element.getAttributeNS(namespace.getUri.orNull, name)
    )
  }

  override def getAttributes(element: Element): Seq[Attribute.Value[String]] = {
    val list: org.w3c.dom.NamedNodeMap = element.getAttributes
    val result = for {
      index <- 0 until list.getLength
      attribute = list.item(index).asInstanceOf[org.w3c.dom.Attr]
      uri = attribute.getNamespaceURI
      if uri != Namespace.Xmlns.uri
      localName = attribute.getLocalName
      prefix = attribute.getPrefix
    } yield Attribute(
      name = localName,
      namespace = Namespace(uri = uri, prefix = prefix)
    ).optional.withValue(Option(attribute.getValue))

    // Dom does not have attribute ordering, so unfortunately the order in the source file
    // gets lost; at least enforce some definitive ordering so that the files do not change
    // on every pretty-printing:
    result.sortBy(_.attribute.qName)
  }

  override protected def setAttribute[T](attribute: Attribute[T], value: T, element: Element): Element = {
    val name: String = attribute.name
    val namespace: Namespace = attribute.namespace
    val valueStr: String = attribute.toString(value)
    if (namespace.isDefault) element.setAttribute(name, valueStr) else {
      // declare the attribute's namespace if it is not declared - unless the attribute *is* a namespace declaration ;)
      if (namespace != Namespace.Xmlns) namespace.ensureDeclared(element)

      element.setAttributeNS(namespace.getUri.orNull, namespace.qName(name), valueStr)
    }
    element
  }

  override def setAttributes(attributes: Attribute.Values, element: Element): Element = ??? // TODO implement

  override def getChildren(element: Element): Nodes = {
    val list: org.w3c.dom.NodeList = element.getChildNodes
    for (index <- 0 until list.getLength) yield list.item(index)
  }
}
