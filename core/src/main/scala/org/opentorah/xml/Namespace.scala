package org.opentorah.xml

/* from https://www.w3.org/TR/xml-names/

  The prefix xml is by definition bound to the namespace name http://www.w3.org/XML/1998/namespace.
  It MAY, but need not, be declared, and MUST NOT be bound to any other namespace name.
  Other prefixes MUST NOT be bound to this namespace name, and it MUST NOT be declared as the default namespace.

  The prefix xmlns is used only to declare namespace bindings and is by definition bound to the namespace
  name http://www.w3.org/2000/xmlns/. It MUST NOT be declared. Other prefixes MUST NOT be bound to this namespace name,
  and it MUST NOT be declared as the default namespace. Element names MUST NOT have the prefix xmlns.

  All other prefixes beginning with the three-letter sequence x, m, l, in any case combination, are reserved.
  This means that:
    users SHOULD NOT use them except as defined by later specifications
    processors MUST NOT treat them as fatal errors.

  If there is no default namespace declaration in scope, the namespace name has no value.
  The namespace name for an unprefixed attribute name always has no value.
*/
sealed trait Namespace derives CanEqual:

  def getUri: Option[String]

  def uri: String

  def getPrefix: Option[String]

  def qName(localName: String): String

  override def toString: String = attributeValue.toString

  override def equals(other: Any): Boolean =
    val that = other.asInstanceOf[Namespace]
    (this.getUri == that.getUri) && (this.getPrefix == that.getPrefix)

  final def isDefault: Boolean = getPrefix.isEmpty

  final def default: Namespace = if isDefault then this else Namespace(prefix = None, uri = getUri)
  
  final def attribute: Attribute.Optional[String] = Attribute(
    name = getPrefix.getOrElse(Namespace.defaultNamespaceAttributeName),
    namespace = Namespace.Xmlns
  ).optional

  final def attributeValue: Attribute.Value[String] = attribute.withValue(getUri)

  def declare(element: Element): Element =
    element.copy(scope = scala.xml.NamespaceBinding(getPrefix.orNull, getUri.get, element.scope))
    
object Namespace:
  // Note: empty string attribute name is used for default namespace attributes;
  // it is processed specially by Namespace.Xmlns.qName()
  private val defaultNamespaceAttributeName: String = ""
  
  def get(element: Element): Namespace = Namespace(
    prefix = element.prefix,
    uri = element.getNamespace(element.prefix)
  )

  private given CanEqual[scala.xml.NamespaceBinding, scala.xml.NamespaceBinding] = CanEqual.derived

  // Note: maybe support re-definitions of the namespace bindings - like in  scala.xml.NamespaceBinding.shadowRedefined()?
  def getAll(element: Element): Seq[Namespace] =
    @scala.annotation.tailrec
    def get(result: Seq[Namespace], namespaceBinding: scala.xml.NamespaceBinding): Seq[Namespace] =
      if namespaceBinding == scala.xml.TopScope then result else
        val namespace: Namespace = Namespace(
          prefix = namespaceBinding.prefix,
          uri = namespaceBinding.uri
        )
        get(result :+ namespace, namespaceBinding.parent)

    get(Seq.empty, element.scope)
    
  def remove(nodes: Nodes): Nodes = nodes.map(remove)

  def remove(node: Node): Node = if !Element.is(node) then node else
    val element = Element.as(node)
    element.copy(scope = scala.xml.TopScope, child = remove(Element.getChildren(element)))

  object Xmlns extends Namespace:
    val prefix: String = "xmlns"
    override def getPrefix: Option[String] = Some(prefix)
    override def uri: String = "http://www.w3.org/2000/xmlns/"
    override def getUri: Option[String] = Some(uri)
    def qName(localName: String): String = 
      prefix + (if localName.isEmpty then defaultNamespaceAttributeName else ":" + localName)

  object No extends Namespace:
    override def toString: String = "<No Namespace>"
    override def getPrefix: Option[String] = None
    override def getUri: Option[String] = None
    override def uri: String = getUri.get

    def qName(localName: String): String =
      require(localName.nonEmpty)
      localName

  private final class Default(override val uri: String) extends Namespace:
    require((uri != null) && uri.nonEmpty)
    override def getPrefix: Option[String] = None
    override def getUri: Option[String] = Some(uri)
    override def qName(localName: String): String =
      require(localName.nonEmpty)
      localName

  class Prefixed(prefix: String, override val uri: String) extends Namespace:
    require((prefix != null) && prefix.nonEmpty)
    require((uri != null) && uri.nonEmpty)
    override def getPrefix: Option[String] = Some(prefix)
    override def getUri: Option[String] = Some(uri)
    override def qName(localName: String): String =
      require(localName.nonEmpty)
      prefix + ":" + localName
      
  object XInclude extends Prefixed(uri = "http://www.w3.org/2001/XInclude", prefix = "xi")

  object XLink extends Prefixed(uri = "http://www.w3.org/1999/xlink", prefix = "xlink")
  
  def apply(
    prefix: Option[String],
    uri: Option[String]
  ): Namespace =
    if prefix.isEmpty && uri.isEmpty then No else
    if prefix.isEmpty && uri.isDefined then Default(uri.get) else
    if uri.isDefined then Prefixed(prefix.get, uri.get) else
      throw IllegalArgumentException(s"prefix [${prefix.get}] without uri!")

  def apply(
    prefix: String,
    uri: String
  ): Namespace = Namespace(
    prefix = Option(prefix),
    uri = Option(uri)
  )

