package org.opentorah.xml

import org.opentorah.util.{Files, Strings}
import org.xml.sax.{InputSource, XMLFilter, XMLReader}
import java.net.URL
import scala.xml.parsing.FactoryAdapter

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

  override def load(
    inputSource: InputSource,
    filters: Seq[XMLFilter],
    resolver: Option[Resolver],
    processIncludes: Xerces.ProcessIncludes = Xerces.ProcessIncludes.YesWithBases
  ): Element = load0(
      inputSource,
      filters,
      resolver,
      processIncludes,
      fixXercesXIncludes = true
  )

  // TODO I'd like the following code to look this way: scala.xml.XML.withSAXParser(...).load(inputSource), but:
  // - I need to filter the input, and XMLFilter only works with XMLReader, not SAXParser;
  // - that is why I call getXMLReader() on the newly created SAXParse in Xerces;
  // - XMLLoader uses SAXParser, not XMLReader;
  // - there is no going back from the XMLReader to SAXParser... (?)
  // So: file a pull request that will change XMLLoader from using SAXParser to XMLReader,
  // which by default should be obtained from the SAXParser :)
  def load0(
    inputSource: InputSource,
    filters: Seq[XMLFilter],
    resolver: Option[Resolver],
    processIncludes: Xerces.ProcessIncludes,
    fixXercesXIncludes: Boolean
  ): Element =
    val xmlReader: XMLReader = Xerces.getXMLReader(
      filters,
      resolver,
      processIncludes,
      logger = Xml.logger // TODO globalize
    )

    val adapter: scala.xml.parsing.FactoryAdapter = scala.xml.parsing.NoBindingFactoryAdapter()
    adapter.scopeStack = scala.xml.TopScope :: adapter.scopeStack

    xmlReader.setContentHandler(adapter)
    xmlReader.setDTDHandler(adapter)
    xmlReader.setProperty("http://xml.org/sax/properties/lexical-handler", adapter)

    xmlReader.parse(inputSource)

    adapter.scopeStack = adapter.scopeStack.tail
    val result: Element = adapter.rootElem.asInstanceOf[Element]

    if !fixXercesXIncludes then result else fixXIncludeBug(result)

  override def isText(node: Node): Boolean = node.isInstanceOf[Text]
  override def asText(node: Node): Text    = node.asInstanceOf[Text]
  override def getText(text: Text): String = text.text

  override def mkText(text: String, seed: Node): Text = mkText(text)
  def mkText(text: String): Text = scala.xml.Text(text)

  override def isComment(node: Node): Boolean = node.isInstanceOf[Comment]
  override def mkComment(text: String, seed: Node): Comment = mkComment(text)
  // TODO add spaces for everything, not just ScalaXml?
  def mkComment(text: String): Comment = scala.xml.Comment(s" $text ")

  override def isElement(node: Node): Boolean = node.isInstanceOf[Element]
  override def asElement(node: Node): Element = node.asInstanceOf[Element]

  override def getName(element: Element): String = element.label
  override def getPrefix(element: Element): Option[String] = Option(element.prefix)
  def rename(element: Element, name: String): Element = element.copy(label = name) // TODO into Xml...

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

  // TODO move up in the hierarchy
  def removeNamespace(nodes: Seq[Node]): Seq[Node] = nodes.map(removeNamespace)
  def removeNamespace(node: Node): Node = if !isElement(node) then node else
    val element = asElement(node)
    element.copy(scope = scala.xml.TopScope, child = removeNamespace(getChildren(element)))

  override def getAttribute(attribute: Attribute[?], attributes: Attributes): Option[String] = {
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
  override def setAttribute[T](attribute: Attribute.Value[T], element: Element): Element = attribute.valueEffective match
    case None => element
    case _ => addAttributes(Seq(attribute), element)

  override def setAttributes(attributes: Attribute.Values, element: Element): Element =
    element.copy(attributes = attributes.foldRight[scala.xml.MetaData](scala.xml.Null)(
      (attributeValue, next) => toMetaData(attributeValue, next))
    )

  private def toMetaData[T](attributeValue: Attribute.Value[T], next: scala.xml.MetaData): scala.xml.MetaData =
    val attribute: Attribute[T] = attributeValue.attribute
    scala.xml.Attribute(
      pre = attribute.namespace.getPrefix.orNull,
      key = attribute.name,
      value = attributeValue.valueEffective.map(mkText).map(Seq(_)).orNull,
      next = next
    )

  override def getChildren(element: Element): Nodes = element.child

  override def setChildren(element: Element, children: Nodes): Element = element.copy(child = children)

  override protected def descendants(nodes: Nodes, elementName: String): Nodes = nodes.flatMap(node => node.flatMap(_ \\ elementName))

  def toNodes(nodes: Nodes): Element.Nodes = Element.Nodes(ScalaXml)(nodes)

  // This is mind-bogglingly weird, but:
  // - Xerces has a bug in the handling of XIncludes;
  // - starting at the third level of nested includes, the values of the xml:base attributes are wrong;
  // - the bug https://issues.apache.org/jira/browse/XERCESJ-1102 was reported in October 2005!!!
  // - a patch that allegedly fixes the issue is known for years
  // - a comment from the Xerces maintainer says:
  //   What Xerces needs most is new contributors / committers who can volunteer their time and help review these patches and get them committed.
  //   We also need a new release. It's been 5 years. Long overdue.
  //   If you or anyone else is interested in getting involved we'd be happy to have you join the project.
  // - latest release of Xerces was in 2023, with the bug still there
  // - many projects depend on Xerces, including Saxon, where the bug was also discussed: https://saxonica.plan.io/issues/4664
  // - allegedly, the bug is "SaxonC 11.1" - although how can this be with Saxon not shipping its own Xerces is not clear.
  //
  // So, I need to process XIncludes myself instead of relying on the industry-standard Xerces!
  // What a nightmare...
  private def fixXIncludeBug(element: ScalaXml.Element): ScalaXml.Element =
    def fix(current: Option[String], level: Int, element: ScalaXml.Element): ScalaXml.Element =
      val base: Option[String] = Xml.baseAttribute.optional.get(ScalaXml)(element)

      val (baseFixed: Option[String], levelNew: Int) = base match
        case None => (current, level)
        case Some(base) =>
          val baseFixed: String = current match
            case None => base
            case Some(current) =>
              val basePath: Seq[String] = Files.splitUrl(base)
              val missing: Seq[String] = Files.splitUrl(current)
                .init // drop the xml file at the end
                .takeWhile(_ != basePath.head)
              if missing.isEmpty then base else
//                println(s"current=$current; prepending missing to $base")
                (missing ++ basePath).mkString("/")
          (Some(baseFixed), level + 1)

      val result: ScalaXml.Element = ScalaXml.setChildren(element,
        for child <- ScalaXml.getChildren(element) yield
          if !ScalaXml.isElement(child) then child else fix(baseFixed, levelNew, ScalaXml.asElement(child))
      )

      if base.isEmpty then result else
        ScalaXml.addAttribute(Xml.baseAttribute.required.withValue(baseFixed.get),
          ScalaXml.removeAttribute(Xml.baseAttribute, result)
        )

    fix(current = None, level = 0, element = element)

  // Since I essentially "fix" xml:base attributes above
  // to be relative to the initial document and not to the including one
  // (which is incorrect, but what else can I do?)
  // I need to supply that initial URL for the subUrl calculations *for ScalaXml only*:
  override def parentBase: Parser[Option[URL]] = Parsing.initialBaseUrl
