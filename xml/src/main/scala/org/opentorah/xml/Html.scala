package org.opentorah.xml

import org.opentorah.util.Files
import zio.URIO
import java.net.URI

object Html extends Dialect with Doctype {

  override val namespace: Namespace = Namespace(uri="http://www.w3.org/1999/xhtml", prefix="xhtml")

  override val mimeType: String = "application/xhtml+xml"

  override val doctype: String = "<!DOCTYPE html>"

  val idAttribute: Attribute[String] = Attribute("id")
  val langAttribute: Attribute[String] = Attribute("lang")
  val classAttribute: Attribute[String] = Attribute("class")

  val reservedElements: Set[String] = Set("head", "body")

  val reservedAttributes: Set[String] = Set("class", "target", "lang")

  final case class a(
    uri: Option[URI] = None,
    target: Option[String] = None,
    id: Option[String] = None,
    classes: Seq[String] = Seq.empty,
    declareNamespace: Boolean = false
  ) {
    def setId(value: String): a = copy(id = Some(value))

    def setTarget(value: String): a = copy(target = Some(value))

    def addClass(value: String): a = copy(classes = classes :+ value)

    def withNamespace: a = copy(declareNamespace = true)

    def setFragment(value: String): a = copy(uri = Some {
      val newUri: URI = uri.getOrElse(a.rootUri)
      new URI(newUri.getScheme, newUri.getUserInfo, newUri.getHost, newUri.getPort, newUri.getPath, newUri.getQuery, value)
    })

    def setQuery(value: String): a = copy(uri = Some {
      val newUri: URI = uri.getOrElse(a.rootUri)
      new URI(newUri.getScheme, newUri.getUserInfo, newUri.getHost, newUri.getPort, newUri.getPath, value, newUri.getFragment)
    })

    def apply(text: String): Xml.Element = apply(Seq(Xml.mkText(text)))

    def apply(element: Xml.Element): Xml.Element = apply(Seq(element))

    def apply(xml: RawXml#Value): Xml.Element = apply(xml.xml)

    def apply(children: Xml.Nodes): Xml.Element = {
      val result: Xml.Element =
        <a
          href={uri.map(_.toString).orNull}
          target={target.orNull}
          class={if (classes.isEmpty) null else classes.mkString(" ")}
          id={id.orNull}
        >{children}</a>

      if (!declareNamespace) result else Html.namespace.default.declare(result)
    }
  }

  object a {
    val empty: a = new a()

    private val rootUri: URI = new URI(null, null, "/", null)

    def apply(path: Seq[String]): a = apply(new URI(null, null, Files.mkUrl(path), null))

    def apply(uri: URI): a = a(uri = Some(uri))
  }

  // Converting other XML dialects (e.g., TEI) to HTML

  private final class EndNote(
    number: Int,
    id: Option[String],
    val content: Xml.Nodes
  ) {
    private def contentId: String = s"_note_$number"

    private def srcId: String = id.getOrElse(s"src_note_$number")

    def link: Xml.Element = a.empty.setFragment(contentId).setId(srcId)(element = <sup>{number}</sup>)

    def body: Xml.Element =
      <span xmlns={Html.namespace.uri} class="endnote" id={contentId}>
        {a.empty.setFragment(srcId).addClass("endnote-backlink")(number.toString)}
        {content}
      </span>
  }

  // TODO use ZIO Layers; split LinkResolver from EndNotes...
  // TODO add pre-existing ids as a set and take it into account when getting a new id (including for notes)
  // TODO add nested section ids
  final class State(val resolver: LinkResolver) {
    private var endNotes: Seq[EndNote] = Seq.empty
    private[Html] def getEndNotes: Seq[EndNote] = endNotes

    // TODO get two ids, one for the actual content at the end
    def addEndNote(id: Option[String], content: Xml.Nodes): Xml.Element = {
      val note: EndNote = new EndNote(
        number = endNotes.length + 1,
        id,
        content
      )
      endNotes = endNotes :+ note
      note.link
    }
  }

  trait To {
    protected def namespace: Namespace

    protected def elementTransform(element: Xml.Element): URIO[State, Xml.Element]

    protected def isEndNote(element: Xml.Element): Boolean

    def toHtml(resolver: LinkResolver, element: Xml.Element): Xml.Element = {
      def runTransform(element: Xml.Element): (Xml.Element, State) = Xml.runTransform(
        transform = Xml.inNamespace[State](
          namespace,
          transform = (element: Xml.Element) => elementTransform(element).map(newElement => transformElement(element, newElement))
        ),
        state = new State(resolver),
        element
      )

      val (result, finalState) = runTransform(element)
      <div xmlns={Html.namespace.uri} class="html">
        {result}{runTransform(
        <div xmlns={Html.namespace.uri} class="endnotes">
          {for (note <- finalState.getEndNotes) yield note.body}
        </div>
      ) /* TODO do not ignore end-notes inside end-notes */ ._1}
      </div>
    }

    private def transformElement(element: Xml.Element, newElement: Xml.Element): Xml.Element = {
      val name: String = element.label
      val attributes: Seq[Attribute.Value[String]] = Xml.getAttributes(element)
      if (newElement eq element) Xml.setAttributes(
        element = if (reservedElements.contains(name)) element.copy(label = addPrefix(name)) else element,
        attributes = attributes.map(transformXmlAttribute)
      ) else Xml.addAttributes(
        element = Html.namespace.default.declare(newElement),
        attributes = Html.classAttribute.required.withValue(name) +: attributes.map(transformAttribute)
      )
    }

    private def transformAttribute(attribute: Attribute.Value[String]): Attribute.Value[String] = {
      val result: Attribute.Value[String] = transformXmlAttribute(attribute)
      val name: String = result.attribute.name
      if (!reservedAttributes.contains(name)) result
      else Attribute(addPrefix(name)).optional.withValue(result.value)
    }

    private def transformXmlAttribute(attribute: Attribute.Value[String]): Attribute.Value[String] =
      // TODO xml:base? xml:space?
      if (attribute.attribute == Xml.idAttribute  ) Html.idAttribute  .optional.withValue(attribute.value) else
      if (attribute.attribute == Xml.langAttribute) Html.langAttribute.optional.withValue(attribute.value) else
        attribute

    private def addPrefix(name: String): String = s"${namespace.getPrefix.get}-$name"
  }
}
