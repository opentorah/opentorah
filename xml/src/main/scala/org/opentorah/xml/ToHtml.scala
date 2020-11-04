package org.opentorah.xml

import zio.{URIO, ZIO}
import scala.xml.Node

trait ToHtml {

  protected def namespace: Namespace

  protected def elementTransform(element: Xml.Element): URIO[State, Xml.Element]

  protected def isEndNote(element: Xml.Element): Boolean

  final protected def link(
    ref: Option[String],
    r: (State, String) => Option[LinkResolver.Resolved],
    children: Seq[Node]
  ): URIO[State, Xml.Element] = ref.map(ref => ZIO.access[State](r(_, ref))).getOrElse(ZIO.none).map(resolved => a(
    id = None,
    href = resolved.map(_.urlAsString).orElse(ref),
    target = resolved.flatMap(_.role),
    children
  ))

  final protected def a(
    id: Option[String],
    href: Option[String],
    target: Option[String],
    children: Seq[Node]
  ): Xml.Element =
    <a id={id.orNull} href={href.orNull} target={target.orNull}>{children}</a>

  final class EndNote(number: Int, id: Option[String], val content: Seq[Node]) {
    private def contentId: String = s"_note_$number"
    private def srcId: String = id.getOrElse(s"src_note_$number")

    def link: Xml.Element =
      <a id={srcId} href={s"#$contentId"}><sup>{number}</sup></a>

    def body: Xml.Element =
      <span xmlns={Xhtml.namespace.uri} class="endnote" id={contentId}>
        <a class="endnote-backlink" href={s"#$srcId"}>{number}</a>
        {content}
      </span>
  }

  // TODO add pre-existing ids as a set and take it into account when getting a new id (including for notes)
  // TODO add nested section ids
  final class State(val resolver: LinkResolver) {
    private var endNotes: Seq[EndNote] = Seq.empty

    // TODO get two ids, one for the actual content at the end
    def addEndNote(id: Option[String], content: Seq[Node]): Xml.Element = {
      val note: EndNote = new EndNote(
        number = endNotes.length + 1,
        id,
        content
      )
      endNotes = endNotes :+ note
      note.link
    }

    def getEndNotes: Seq[EndNote] = endNotes
  }

  def toHtml(resolver: LinkResolver, element: Xml.Element): Xml.Element = {
    val (result, finalState) = runTransform(resolver, element)
    <div xmlns={Xhtml.namespace.uri} class="html">
      {result}
      {runTransform(resolver,
      <div xmlns={Xhtml.namespace.uri} class="endnotes">
        {for (note <- finalState.getEndNotes) yield note.body}
      </div>
      )/* TODO do not ignore end-notes inside end-notes */._1}
    </div>
  }

  private def runTransform(resolver: LinkResolver, element: Xml.Element): (Xml.Element, State) = Xml.runTransform(
    Xml.inNamespace[State](namespace, liftedElementTransform),
    new State(resolver),
    element
  )

  private val htmlElementNames: Set[String] = Set("head", "body")

  private def liftedElementTransform(element: Xml.Element): URIO[State, Xml.Element] = for {
    newElement <- elementTransform(element)
  } yield {
    val name: String = element.label
    val attributes: Seq[Attribute.Value[String]] = Xml.getAttributes(element)
    if (newElement eq element) Xml.setAttributes(
      element = if (htmlElementNames.contains(name)) element.copy(label = addPrefix(name)) else element,
      attributes = attributes.map(transformXmlAttribute)
    ) else Xml.addAttributes(
      element = Xhtml.namespace.default.declare(newElement),
      attributes = Xhtml.classAttribute.withValue(name) +: attributes.map(transformAttribute)
    )
  }

  private val htmlAttributeNames: Set[String] = Set("class", "target", "lang")

  private def transformAttribute(attribute: Attribute.Value[String]): Attribute.Value[String] = {
    val result: Attribute.Value[String] = transformXmlAttribute(attribute)
    val name: String = result.attribute.name
    if (htmlAttributeNames.contains(name))
      Attribute(addPrefix(name)).withOptionalValue(result.value)
    else
      result
  }

  private def transformXmlAttribute(attribute: Attribute.Value[String]): Attribute.Value[String] = {
    // TODO xml:base? xml:space?
    if (attribute.attribute == Xml.idAttribute  ) Xhtml.idAttribute  .withOptionalValue(attribute.value) else
    if (attribute.attribute == Xml.langAttribute) Xhtml.langAttribute.withOptionalValue(attribute.value) else
      attribute
  }

  private def addPrefix(name: String): String = s"${namespace.getPrefix.get}-$name"
}
